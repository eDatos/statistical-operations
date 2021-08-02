package org.siemac.metamac.statistical.operations.web.server;

import org.siemac.metamac.statistical.operations.web.server.handlers.GetOperationsListsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFamilyPaginatedListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFamilyActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFamilyAndOperationsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.DeleteFamilyListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetOperationPaginatedListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetOperationAndInstancesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.DeleteOperationListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.ReSendStreamMessageOperationActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetInstanceActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.DeleteInstanceListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetInstanceListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetHelpUrlActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.PublishExternallyFamilyActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.PublishExternallyInstanceActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.PublishExternallyOperationActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.PublishInternallyFamilyActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.PublishInternallyInstanceActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.PublishInternallyOperationActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.SaveFamilyActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.SaveInstanceActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.SaveOperationActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.UpdateFamilyOperationsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.UpdateInstancesOrderActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.UpdateOperationFamiliesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.ValidateTicketActionHandler;

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
