package org.siemac.metamac.statistical.operations.web.server;

import org.siemac.metamac.statistical.operations.web.server.handlers.DeleteFamilyListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.DeleteInstanceListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.DeleteOperationListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.FindAllCategorySchemesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.FindAllCodeListsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.FindAllCommonMetadataActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.FindAllConceptSchemesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.FindAllOrganisationSchemesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetCategoriesFromSchemeActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetCodesFromCodeListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetConceptsFromSchemeActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFamilyActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFamilyAndOperationsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFamilyListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetFrequencyCodesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetInstanceActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetInstanceListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetOperationAndInstancesActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetOperationListActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetOperationsListsActionHandler;
import org.siemac.metamac.statistical.operations.web.server.handlers.GetOrganisationsFromSchemeActionHandler;
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
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCodeListsAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCommonMetadataAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllConceptSchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllOrganisationSchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCodesFromCodeListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetConceptsFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFrequencyCodesAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationsListsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOrganisationsFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateFamilyOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateInstancesOrderAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesAction;
import org.siemac.metamac.web.common.server.handlers.CloseSessionActionHandler;
import org.siemac.metamac.web.common.server.handlers.GetLoginPageUrlActionHandler;
import org.siemac.metamac.web.common.server.handlers.MockCASUserActionHandler;
import org.siemac.metamac.web.common.shared.CloseSessionAction;
import org.siemac.metamac.web.common.shared.GetLoginPageUrlAction;
import org.siemac.metamac.web.common.shared.MockCASUserAction;
import org.siemac.metamac.web.common.shared.ValidateTicketAction;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.spring.HandlerModule;

/**
 * Module which binds the handlers and configurations.
 */
@Component
public class ServerModule extends HandlerModule {

    public ServerModule() {
    }

    protected void configureHandlers() {
        bindHandler(GetFamilyListAction.class, GetFamilyListActionHandler.class);
        bindHandler(GetFamilyAction.class, GetFamilyActionHandler.class);
        bindHandler(GetFamilyAndOperationsAction.class, GetFamilyAndOperationsActionHandler.class);
        bindHandler(SaveFamilyAction.class, SaveFamilyActionHandler.class);
        bindHandler(DeleteFamilyListAction.class, DeleteFamilyListActionHandler.class);
        bindHandler(SaveOperationAction.class, SaveOperationActionHandler.class);
        bindHandler(GetOperationAndInstancesAction.class, GetOperationAndInstancesActionHandler.class);
        bindHandler(SaveInstanceAction.class, SaveInstanceActionHandler.class);
        bindHandler(GetOperationsListsAction.class, GetOperationsListsActionHandler.class);
        bindHandler(GetInstanceAction.class, GetInstanceActionHandler.class);
        bindHandler(GetOperationListAction.class, GetOperationListActionHandler.class);
        bindHandler(DeleteOperationListAction.class, DeleteOperationListActionHandler.class);
        bindHandler(DeleteInstanceListAction.class, DeleteInstanceListActionHandler.class);
        bindHandler(GetInstanceListAction.class, GetInstanceListActionHandler.class);
        bindHandler(UpdateFamilyOperationsAction.class, UpdateFamilyOperationsActionHandler.class);
        bindHandler(UpdateOperationFamiliesAction.class, UpdateOperationFamiliesActionHandler.class);
        bindHandler(PublishInternallyFamilyAction.class, PublishInternallyFamilyActionHandler.class);
        bindHandler(PublishExternallyFamilyAction.class, PublishExternallyFamilyActionHandler.class);
        bindHandler(PublishInternallyOperationAction.class, PublishInternallyOperationActionHandler.class);
        bindHandler(PublishExternallyOperationAction.class, PublishExternallyOperationActionHandler.class);
        bindHandler(PublishInternallyInstanceAction.class, PublishInternallyInstanceActionHandler.class);
        bindHandler(PublishExternallyInstanceAction.class, PublishExternallyInstanceActionHandler.class);
        bindHandler(FindAllCategorySchemesAction.class, FindAllCategorySchemesActionHandler.class);
        bindHandler(GetCategoriesFromSchemeAction.class, GetCategoriesFromSchemeActionHandler.class);
        bindHandler(FindAllOrganisationSchemesAction.class, FindAllOrganisationSchemesActionHandler.class);
        bindHandler(GetOrganisationsFromSchemeAction.class, GetOrganisationsFromSchemeActionHandler.class);
        bindHandler(FindAllCommonMetadataAction.class, FindAllCommonMetadataActionHandler.class);
        bindHandler(FindAllConceptSchemesAction.class, FindAllConceptSchemesActionHandler.class);
        bindHandler(GetConceptsFromSchemeAction.class, GetConceptsFromSchemeActionHandler.class);
        bindHandler(FindAllCodeListsAction.class, FindAllCodeListsActionHandler.class);
        bindHandler(GetCodesFromCodeListAction.class, GetCodesFromCodeListActionHandler.class);
        bindHandler(GetFrequencyCodesAction.class, GetFrequencyCodesActionHandler.class);
        bindHandler(UpdateInstancesOrderAction.class, UpdateInstancesOrderActionHandler.class);

        bindHandler(ValidateTicketAction.class, ValidateTicketActionHandler.class);
        bindHandler(GetLoginPageUrlAction.class, GetLoginPageUrlActionHandler.class);
        bindHandler(CloseSessionAction.class, CloseSessionActionHandler.class);

        // This action should be removed to use CAS authentication
        bindHandler(MockCASUserAction.class, MockCASUserActionHandler.class);
    }

}
