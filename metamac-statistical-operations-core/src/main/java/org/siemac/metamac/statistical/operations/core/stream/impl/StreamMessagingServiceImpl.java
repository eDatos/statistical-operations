package org.siemac.metamac.statistical.operations.core.stream.impl;

import java.util.Properties;

import org.apache.avro.specific.SpecificRecordBase;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.conf.StatisticalOperationsConfigurationService;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.stream.StreamMessagingService;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.mappers.impl.OperationDo2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.OperationAvro;
import org.siemac.metamac.statistical.operations.web.server.stream.AvroMessage;
import org.siemac.metamac.statistical.operations.web.server.stream.KafkaCustomProducer;
import org.siemac.metamac.statistical.operations.web.server.stream.MessageBase;
import org.siemac.metamac.statistical.operations.web.server.stream.ProducerBase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.stereotype.Component;

import static io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG;
import static org.siemac.edatos.core.common.constants.shared.ConfigurationConstants.KAFKA_BOOTSTRAP_SERVERS;
import static org.siemac.edatos.core.common.constants.shared.ConfigurationConstants.KAFKA_SCHEMA_REGISTRY_URL;

@Component
public class StreamMessagingServiceImpl<K, V extends SpecificRecordBase> implements StreamMessagingService, ApplicationListener<ContextClosedEvent> {
    @Autowired
    private StatisticalOperationsConfigurationService statisticalOperationsConfigurationService;

    @Autowired
    private OperationDo2AvroMapper operationAvroMapper;

    private static final String CONSUMER_QUERY_1_NAME = "statistical_operations_producer_1";
    private ProducerBase<K, V> producer;

    @Override
    public void sendMessage(Operation operation) throws MetamacException {
        if (operation == null) {
            return;
        }

        OperationAvro operationAvro = operationAvroMapper.toAvro(operation);
        String key = operation.getUrn();

        // FIXME(EDATOS-3380): Unchecked casts???????????
        @SuppressWarnings("unchecked") MessageBase<K, V> message = new AvroMessage<>((K) key, (V) operationAvro);
        String topic = statisticalOperationsConfigurationService.retrieveKafkaTopicOperationsPublication();

        sendMessage(message, topic);
    }

    private ProducerBase<K, V> getProducer() throws MetamacException {
        if (producer == null) {
            producer = new KafkaCustomProducer<>(getProducerProperties());
        }
        return producer;
    }

    private void sendMessage(MessageBase<K, V> message, String topic) throws MetamacException {
        getProducer().sendMessage(message, topic);
    }

    private Properties getProducerProperties() throws MetamacException {
        Properties props = new Properties();

        String bootstrapServers = statisticalOperationsConfigurationService.retrieveProperty(KAFKA_BOOTSTRAP_SERVERS);
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);

        props.put(ProducerConfig.CLIENT_ID_CONFIG, CONSUMER_QUERY_1_NAME);
        props.put(ProducerConfig.ACKS_CONFIG, "all");
        props.put(ProducerConfig.COMPRESSION_TYPE_CONFIG, "gzip");
        props.put(ProducerConfig.RETRIES_CONFIG, 10);
        props.put(ProducerConfig.MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 1);

        String kafkaSchemaRegistryUrl = statisticalOperationsConfigurationService.retrieveProperty(KAFKA_SCHEMA_REGISTRY_URL);
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
}
