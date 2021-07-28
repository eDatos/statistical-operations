package org.siemac.metamac.statistical.operations.web.server;

import org.siemac.metamac.statistical.operations.web.server.handlers.*;
import org.siemac.metamac.statistical.operations.web.server.handlers.external.GetCommonMetadataConfigurationsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.external.GetExternalResourcesActionHandler;
import org.siemac.metamac.statistical.operations.web.shared.*;
import org.siemac.metamac.statistical.operations.web.shared.external.GetCommonMetadataConfigurationsAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesAction;
import org.siemac.metamac.web.common.server.handlers.*;
import org.siemac.metamac.web.common.shared.*;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.spring.HandlerModule;

/**
 * Module which binds the handlers and configurations.
 */
@Component
public class ServerModule extends HandlerModule {

    public ServerModule() {
    }

    @Override
    protected void configureHandlers() {

        // Families
        bindHandler(GetFamilyPaginatedListAction.class, GetFamilyPaginatedListActionHandler.class);
        bindHandler(GetFamilyAction.class, GetFamilyActionHandler.class);
        bindHandler(GetFamilyAndOperationsAction.class, GetFamilyAndOperationsActionHandler.class);
        bindHandler(SaveFamilyAction.class, SaveFamilyActionHandler.class);
        bindHandler(DeleteFamilyListAction.class, DeleteFamilyListActionHandler.class);
        bindHandler(UpdateFamilyOperationsAction.class, UpdateFamilyOperationsActionHandler.class);
        bindHandler(PublishInternallyFamilyAction.class, PublishInternallyFamilyActionHandler.class);
        bindHandler(PublishExternallyFamilyAction.class, PublishExternallyFamilyActionHandler.class);

        // Operations
        bindHandler(GetOperationPaginatedListAction.class, GetOperationPaginatedListActionHandler.class);
        bindHandler(GetOperationsListsAction.class, GetOperationsListsActionHandler.class);
        bindHandler(SaveOperationAction.class, SaveOperationActionHandler.class);
        bindHandler(GetOperationAndInstancesAction.class, GetOperationAndInstancesActionHandler.class);
        bindHandler(DeleteOperationListAction.class, DeleteOperationListActionHandler.class);
        bindHandler(UpdateOperationFamiliesAction.class, UpdateOperationFamiliesActionHandler.class);
        bindHandler(PublishInternallyOperationAction.class, PublishInternallyOperationActionHandler.class);
        bindHandler(PublishExternallyOperationAction.class, PublishExternallyOperationActionHandler.class);
        bindHandler(ReSendStreamMessageOperationAction.class, ReSendStreamMessageOperationActionHandler.class);

        // Instances
        bindHandler(SaveInstanceAction.class, SaveInstanceActionHandler.class);
        bindHandler(GetInstanceAction.class, GetInstanceActionHandler.class);
        bindHandler(DeleteInstanceListAction.class, DeleteInstanceListActionHandler.class);
        bindHandler(GetInstanceListAction.class, GetInstanceListActionHandler.class);
        bindHandler(PublishInternallyInstanceAction.class, PublishInternallyInstanceActionHandler.class);
        bindHandler(PublishExternallyInstanceAction.class, PublishExternallyInstanceActionHandler.class);
        bindHandler(UpdateInstancesOrderAction.class, UpdateInstancesOrderActionHandler.class);

        bindHandler(GetCommonMetadataConfigurationsAction.class, GetCommonMetadataConfigurationsActionHandler.class);

        // External
        bindHandler(GetExternalResourcesAction.class, GetExternalResourcesActionHandler.class);

        bindHandler(ValidateTicketAction.class, ValidateTicketActionHandler.class);
        bindHandler(GetLoginPageUrlAction.class, GetLoginPageUrlActionHandler.class);
        bindHandler(CloseSessionAction.class, CloseSessionActionHandler.class);
        bindHandler(GetNavigationBarUrlAction.class, GetNavigationBarUrlActionHandler.class);

        bindHandler(LoadConfigurationPropertiesAction.class, LoadConfigurationPropertiesActionHandler.class);
        bindHandler(GetHelpUrlAction.class, GetHelpUrlActionHandler.class);

        // This action should be removed to use CAS authentication
        bindHandler(MockCASUserAction.class, MockCASUserActionHandler.class);
    }
}
