package org.siemac.metamac.statistical.operations.core.serviceimpl;

import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.apache.avro.specific.SpecificRecordBase;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StreamMessageStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.serviceimpl.result.SendStreamMessageResult;
import org.siemac.metamac.statistical.operations.core.stream.mappers.impl.OperationDo2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.OperationAvro;
import org.siemac.metamac.statistical.operations.web.server.stream.AvroMessage;
import org.siemac.metamac.statistical.operations.web.server.stream.KafkaCustomProducer;
import org.siemac.metamac.statistical.operations.web.server.stream.MessageBase;
import org.siemac.metamac.statistical.operations.web.server.stream.ProducerBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import static io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG;
import static org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.criteriaFor;
import static org.siemac.edatos.core.common.constants.shared.ConfigurationConstants.KAFKA_BOOTSTRAP_SERVERS;
import static org.siemac.edatos.core.common.constants.shared.ConfigurationConstants.KAFKA_SCHEMA_REGISTRY_URL;

/**
 * Implementation of StreamMessagingService.
 */
@Service("streamMessagingService")
public class StreamMessagingServiceImpl extends StreamMessagingServiceImplBase implements ApplicationListener<ContextClosedEvent> {
    private static final Logger LOGGER = LoggerFactory.getLogger(StreamMessagingServiceImpl.class);
    private static final String CONSUMER_QUERY_1_NAME = "statistical_operations_producer_1";

    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private OperationDo2AvroMapper operationAvroMapper;

    @Autowired
    @Qualifier("txManager")
    private PlatformTransactionManager platformTransactionManager;

    private ProducerBase<Object, SpecificRecordBase> producer;

    @Override
    public SendStreamMessageResult sendMessage(ServiceContext ctx, Operation operation) {
        try {
            updateMessageStatus(operation, StreamMessageStatusEnum.PENDING);
            sendOperation(operation);
            updateMessageStatus(operation, StreamMessageStatusEnum.SENT);
            return new SendStreamMessageResult(operation.getStreamMessageStatus(), null);
        } catch (MetamacException e) {
            updateMessageStatus(operation, StreamMessageStatusEnum.FAILED);
            return new SendStreamMessageResult(operation.getStreamMessageStatus(), Collections.singletonList(new MetamacException(e, ServiceExceptionType.UNABLE_TO_SEND_STREAM_MESSAGING_TO_STREAM_MESSAGING_SERVER)));
        }
    }

    @Override
    public void resendAllPendingAndFailedMessages(ServiceContext ctx) throws MetamacException {
        LOGGER.debug("Checking if there are failed or pending operations waiting to be sent trough Kafka...");
        // @formatter:off
        List<ConditionalCriteria> criteria = criteriaFor(Operation.class)
            .lbrace()
                .withProperty(OperationProperties.streamMessageStatus()).eq(StreamMessageStatusEnum.PENDING)
                .or()
                .withProperty(OperationProperties.streamMessageStatus()).eq(StreamMessageStatusEnum.FAILED)
            .rbrace()
            .and()
            .withProperty(OperationProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY)
            .build();
        // @formatter:on
        List<Operation> operations = getStatisticalOperationsBaseService().findOperationByCondition(ctx, criteria);
        if (operations.isEmpty()) {
            LOGGER.debug("There aren't any operations to be sent through Kafka");
        } else {
            LOGGER.info("{} operations are going to be sent through Kafka", operations.size());
            int sentOperations = 0;
            for (Operation operation : operations) {
                try {
                    LOGGER.debug("Sending operation with code '{}'", operation.getCode());
                    SendStreamMessageResult result = getTransactionTemplate().execute(new MetamacExceptionTransactionCallback<SendStreamMessageResult>() {

                        @Override
                        protected SendStreamMessageResult doInMetamacTransaction(TransactionStatus status) {
                            return sendMessage(ctx, operation);
                        }
                    });
                    if (result.isOk()) {
                        sentOperations++;
                    } else {
                        LOGGER.warn("An error occurred while trying to send through Kafka the operation with code '{}'", operation.getCode(), result.getMainException());
                    }
                } catch (Exception e) {
                    LOGGER.warn("An error occurred while trying to send through Kafka the operation with code '{}'", operation.getCode(), e);
                }
            }
            if (sentOperations == operations.size()) {
                LOGGER.info("All operations sent");
            } else {
                LOGGER.warn("Sent {} of {} operations, check log for more info", sentOperations, operations.size());
            }
        }
    }

    private void updateMessageStatus(Operation operation, StreamMessageStatusEnum status) {
        if (operation != null) {
            operation.setStreamMessageStatus(status);
        }
    }

    private void sendOperation(Operation operation) throws MetamacException {
        if (operation == null) {
            return;
        }

        OperationAvro operationAvro = operationAvroMapper.toAvro(operation);
        String key = operation.getUrn();

        MessageBase<Object, SpecificRecordBase> message = new AvroMessage<>(key, operationAvro);
        String topic = configurationService.retrieveKafkaTopicOperationsPublication();

        sendMessageToTopic(message, topic);
    }

    private void sendMessageToTopic(MessageBase<Object, SpecificRecordBase> message, String topic) throws MetamacException {
        getProducer().sendMessage(message, topic);
    }

    private ProducerBase<Object, SpecificRecordBase> getProducer() throws MetamacException {
        if (producer == null) {
            producer = new KafkaCustomProducer<>(getProducerProperties());
        }
        return producer;
    }

    private Properties getProducerProperties() throws MetamacException {
        Properties props = new Properties();

        String bootstrapServers = configurationService.retrieveProperty(KAFKA_BOOTSTRAP_SERVERS);
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);

        props.put(ProducerConfig.CLIENT_ID_CONFIG, CONSUMER_QUERY_1_NAME);
        props.put(ProducerConfig.ACKS_CONFIG, "all");
        props.put(ProducerConfig.COMPRESSION_TYPE_CONFIG, "gzip");
        props.put(ProducerConfig.RETRIES_CONFIG, 10);
        props.put(ProducerConfig.MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 1);

        String kafkaSchemaRegistryUrl = configurationService.retrieveProperty(KAFKA_SCHEMA_REGISTRY_URL);
        props.put(SCHEMA_REGISTRY_URL_CONFIG, kafkaSchemaRegistryUrl);

        return props;
    }

    @Override
    public void onApplicationEvent(ContextClosedEvent event) {
        if (producer != null) {
            producer.close();
            producer = null;
        }
    }

    private TransactionTemplate getTransactionTemplate() {
        TransactionTemplate transactionTemplate = new TransactionTemplate(platformTransactionManager);
        transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
        return transactionTemplate;
    }

    abstract static class MetamacExceptionTransactionCallback<T> implements TransactionCallback<T> {
        public final T doInTransaction(TransactionStatus status) {
            try {
                return doInMetamacTransaction(status);
            } catch (MetamacException e) {
                throw new RuntimeException("Error in transactional method", e);
            }
        }

        protected abstract T doInMetamacTransaction(TransactionStatus status) throws MetamacException;
    }
}
