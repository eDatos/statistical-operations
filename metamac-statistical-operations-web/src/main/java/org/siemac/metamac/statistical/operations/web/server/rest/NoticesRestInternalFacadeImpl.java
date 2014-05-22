package org.siemac.metamac.statistical.operations.web.server.rest;

import java.util.Locale;

import javax.ws.rs.core.Response;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.lang.LocaleUtil;
import org.siemac.metamac.core.common.util.ServiceContextUtils;
import org.siemac.metamac.rest.notices.v1_0.domain.Message;
import org.siemac.metamac.rest.notices.v1_0.domain.Notice;
import org.siemac.metamac.rest.notices.v1_0.domain.enume.MetamacApplicationsEnum;
import org.siemac.metamac.rest.notices.v1_0.domain.utils.MessageBuilder;
import org.siemac.metamac.rest.notices.v1_0.domain.utils.NoticeBuilder;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.ResourceInternal;
import org.siemac.metamac.statistical.operations.core.conf.StatisticalOperationsConfigurationService;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.mapper.Dto2DoMapper;
import org.siemac.metamac.statistical.operations.core.notices.ServiceNoticeAction;
import org.siemac.metamac.statistical.operations.core.notices.ServiceNoticeMessage;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.mapper.Do2RestInternalMapperV10;
import org.siemac.metamac.web.common.server.rest.utils.RestExceptionUtils;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component(NoticesRestInternalService.BEAN_ID)
public class NoticesRestInternalFacadeImpl implements NoticesRestInternalService {

    @Autowired
    private RestApiLocator                    restApiLocator;

    @Autowired
    private RestExceptionUtils                restExceptionUtils;

    @Autowired
    StatisticalOperationsConfigurationService configurationService;

    @Autowired
    Dto2DoMapper                              dto2DoMapper;

    @Autowired
    Do2RestInternalMapperV10                  do2RestInternalMapperV10;

    // ---------------------------------------------------------------------------------
    // NOTIFICATIONS
    // ---------------------------------------------------------------------------------

    @Override
    public void createNotificationForPublishExternallyOperation(ServiceContext ctx, OperationDto operation) throws MetamacWebException {
        org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal resource = operationDtoToResourceInternal(ctx, operation);
        org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal[] noticeResource = {resource};

        createNotification(ctx, ServiceNoticeAction.OPERATION_PUBLISH_EXTERNALLY, ServiceNoticeMessage.OPERATION_PUBLISH_EXTERNALLY_OK, noticeResource);
    }

    @Override
    public void createNotificationForPublishInternallyOperation(ServiceContext ctx, OperationDto operation) throws MetamacWebException {
        org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal resource = operationDtoToResourceInternal(ctx, operation);
        org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal[] noticeResource = {resource};

        createNotification(ctx, ServiceNoticeAction.OPERATION_PUBLISH_INTERNALLY, ServiceNoticeMessage.OPERATION_PUBLISH_INTERNALLY_OK, noticeResource);
    }

    private void createNotification(ServiceContext ctx, String actionCode, String messageCode, org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal[] resources) throws MetamacWebException {
        Locale locale = ServiceContextUtils.getLocale(ctx);
        String localisedAction = LocaleUtil.getMessageForCode(actionCode, locale);
        String localisedMessage = LocaleUtil.getMessageForCode(messageCode, locale);

        String sendingApp = MetamacApplicationsEnum.GESTOR_OPERACIONES.getName();
        String subject = "[" + sendingApp + "] " + localisedAction;

        // @formatter:off
            Message message = MessageBuilder.message()
                                            .withText(localisedMessage)
                                            .withResources(resources)
                                            .build();
            
            Notice notification = NoticeBuilder.notification()
                                                .withMessages(message)
                                                .withSendingApplication(sendingApp)
                                                .withSendingUser(ctx.getUserId())
                                                .withSubject(subject)
                                                .build();
            // @formatter:on

        Response response = restApiLocator.getNoticesRestInternalFacadeV10().createNotice(notification);
        restExceptionUtils.checkSendNotificationRestResponseAndThrowErrorIfApplicable(ctx, response);
    }

    private org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal operationDtoToResourceInternal(ServiceContext ctx, OperationDto operationDto) throws MetamacWebException {
        Operation operation;
        try {
            operation = dto2DoMapper.operationDtoToEntity(operationDto, ctx);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
        ResourceInternal operationResource = do2RestInternalMapperV10.toResource(operation);
        org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal noticeResource = toNoticeResourceInternal(operationResource);
        return noticeResource;
    }

    private org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal toNoticeResourceInternal(ResourceInternal operationResource) {
        org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal resource = new org.siemac.metamac.rest.notices.v1_0.domain.ResourceInternal();

        if (operationResource != null) {
            resource.setId(operationResource.getId());
            resource.setKind(operationResource.getKind());
            resource.setManagementAppLink(operationResource.getManagementAppLink());
            resource.setName(operationResource.getName());
            resource.setNestedId(operationResource.getNestedId());
            resource.setSelfLink(operationResource.getSelfLink());
            resource.setUrn(operationResource.getUrn());
            resource.setUrnProvider(null);
        }
        return resource;
    }

}
