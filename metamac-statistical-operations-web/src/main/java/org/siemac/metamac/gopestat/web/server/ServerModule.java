package org.siemac.metamac.gopestat.web.server;

import org.siemac.metamac.gopestat.web.server.handlers.DeleteFamilyListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.DeleteInstanceListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.DeleteOperationListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.FindAllCategorySchemesActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.FindAllCodeListsActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.FindAllCommonMetadataActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.FindAllConceptSchemesActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.FindAllOrganisationSchemesActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetCategoriesFromSchemeActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetCodesFromCodeListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetConceptsFromSchemeActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetFamilyActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetFamilyAndOperationsActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetFamilyListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetFrequencyCodesActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetGopestatListsActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetInstanceActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetInstanceListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetOperationAndInstancesActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetOperationListActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.GetOrganisationsFromSchemeActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.PublishExternallyFamilyActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.PublishExternallyInstanceActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.PublishExternallyOperationActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.PublishInternallyFamilyActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.PublishInternallyInstanceActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.PublishInternallyOperationActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.SaveFamilyActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.SaveInstanceActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.SaveOperationActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.UpdateFamilyOperationsActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.UpdateInstancesOrderActionHandler;
import org.siemac.metamac.gopestat.web.server.handlers.UpdateOperationFamiliesActionHandler;
import org.siemac.metamac.gopestat.web.shared.DeleteFamilyListAction;
import org.siemac.metamac.gopestat.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.gopestat.web.shared.DeleteOperationListAction;
import org.siemac.metamac.gopestat.web.shared.FindAllCategorySchemesAction;
import org.siemac.metamac.gopestat.web.shared.FindAllCodeListsAction;
import org.siemac.metamac.gopestat.web.shared.FindAllCommonMetadataAction;
import org.siemac.metamac.gopestat.web.shared.FindAllConceptSchemesAction;
import org.siemac.metamac.gopestat.web.shared.FindAllOrganisationSchemesAction;
import org.siemac.metamac.gopestat.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.gopestat.web.shared.GetCodesFromCodeListAction;
import org.siemac.metamac.gopestat.web.shared.GetConceptsFromSchemeAction;
import org.siemac.metamac.gopestat.web.shared.GetFamilyAction;
import org.siemac.metamac.gopestat.web.shared.GetFamilyAndOperationsAction;
import org.siemac.metamac.gopestat.web.shared.GetFamilyListAction;
import org.siemac.metamac.gopestat.web.shared.GetFrequencyCodesAction;
import org.siemac.metamac.gopestat.web.shared.GetGopestatListsAction;
import org.siemac.metamac.gopestat.web.shared.GetInstanceAction;
import org.siemac.metamac.gopestat.web.shared.GetInstanceListAction;
import org.siemac.metamac.gopestat.web.shared.GetOperationAndInstancesAction;
import org.siemac.metamac.gopestat.web.shared.GetOperationListAction;
import org.siemac.metamac.gopestat.web.shared.GetOrganisationsFromSchemeAction;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyFamilyAction;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.gopestat.web.shared.PublishInternallyFamilyAction;
import org.siemac.metamac.gopestat.web.shared.PublishInternallyInstanceAction;
import org.siemac.metamac.gopestat.web.shared.PublishInternallyOperationAction;
import org.siemac.metamac.gopestat.web.shared.SaveFamilyAction;
import org.siemac.metamac.gopestat.web.shared.SaveInstanceAction;
import org.siemac.metamac.gopestat.web.shared.SaveOperationAction;
import org.siemac.metamac.gopestat.web.shared.UpdateFamilyOperationsAction;
import org.siemac.metamac.gopestat.web.shared.UpdateInstancesOrderAction;
import org.siemac.metamac.gopestat.web.shared.UpdateOperationFamiliesAction;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import com.gwtplatform.dispatch.server.actionvalidator.ActionValidator;
import com.gwtplatform.dispatch.server.spring.HandlerModule;
import com.gwtplatform.dispatch.server.spring.actionvalidator.DefaultActionValidator;
import com.gwtplatform.dispatch.server.spring.configuration.DefaultModule;

/**
 * Module which binds the handlers and configurations.
 */
@Configuration
@Import(DefaultModule.class)
public class ServerModule extends HandlerModule {

    public ServerModule() {
    }

    @Bean
    public ActionValidator getDefaultActionValidator() {
        return new DefaultActionValidator();
    }

    @Bean
    public GetFamilyListActionHandler getFamilyListActionHandler() {
        return new GetFamilyListActionHandler();
    }

    @Bean
    public GetFamilyActionHandler getFamilyActionHandler() {
        return new GetFamilyActionHandler();
    }

    @Bean
    public GetFamilyAndOperationsActionHandler getFamilyAndOperationsActionHandler() {
        return new GetFamilyAndOperationsActionHandler();
    }

    @Bean
    public SaveFamilyActionHandler getSaveFamilyActionHandler() {
        return new SaveFamilyActionHandler();
    }

    @Bean
    public DeleteFamilyListActionHandler getDeleteFamilyListActionHandler() {
        return new DeleteFamilyListActionHandler();
    }

    @Bean
    public SaveOperationActionHandler getSaveOperationActionHandler() {
        return new SaveOperationActionHandler();
    }

    @Bean
    public GetOperationAndInstancesActionHandler getOperationAndInstancesActionHandler() {
        return new GetOperationAndInstancesActionHandler();
    }

    @Bean
    public SaveInstanceActionHandler getSaveInstanceActionHandler() {
        return new SaveInstanceActionHandler();
    }

    @Bean
    public GetGopestatListsActionHandler getGopestatListsActionHandler() {
        return new GetGopestatListsActionHandler();
    }

    @Bean
    public GetInstanceActionHandler getInstanceActionHandler() {
        return new GetInstanceActionHandler();
    }

    @Bean
    public GetOperationListActionHandler getOperationListActionHandler() {
        return new GetOperationListActionHandler();
    }

    @Bean
    public DeleteOperationListActionHandler getDeleteOperationListActionHandler() {
        return new DeleteOperationListActionHandler();
    }

    @Bean
    public DeleteInstanceListActionHandler getDeleteInstanceListActionHandler() {
        return new DeleteInstanceListActionHandler();
    }

    @Bean
    public GetInstanceListActionHandler getInstanceListActionHandler() {
        return new GetInstanceListActionHandler();
    }

    @Bean
    public UpdateFamilyOperationsActionHandler getUpdateFamilyOperationsActionHandler() {
        return new UpdateFamilyOperationsActionHandler();
    }

    @Bean
    public UpdateOperationFamiliesActionHandler getOperationFamiliesActionHandler() {
        return new UpdateOperationFamiliesActionHandler();
    }

    @Bean
    public PublishInternallyFamilyActionHandler getPublishInternallyFamilyActionHandler() {
        return new PublishInternallyFamilyActionHandler();
    }

    @Bean
    public PublishExternallyFamilyActionHandler getPublishExternallyFamilyActionHandler() {
        return new PublishExternallyFamilyActionHandler();
    }

    @Bean
    public PublishInternallyOperationActionHandler getPublishInternallyOperationActionHandler() {
        return new PublishInternallyOperationActionHandler();
    }

    @Bean
    public PublishExternallyOperationActionHandler getPublishExternallyOperationActionHandler() {
        return new PublishExternallyOperationActionHandler();
    }

    @Bean
    public PublishInternallyInstanceActionHandler getPublishInternallyInstanceActionHandler() {
        return new PublishInternallyInstanceActionHandler();
    }

    @Bean
    public PublishExternallyInstanceActionHandler getPublishExternallyInstanceActionHandler() {
        return new PublishExternallyInstanceActionHandler();
    }

    @Bean
    public FindAllCategorySchemesActionHandler getFindAllCategorySchemesActionHandler() {
        return new FindAllCategorySchemesActionHandler();
    }

    @Bean
    public GetCategoriesFromSchemeActionHandler getCategoriesFromSchemeActionHandler() {
        return new GetCategoriesFromSchemeActionHandler();
    }

    @Bean
    public FindAllOrganisationSchemesActionHandler getFindAllOrganisationSchemesActionHandler() {
        return new FindAllOrganisationSchemesActionHandler();
    }

    @Bean
    public GetOrganisationsFromSchemeActionHandler getGetOrganisationsFromSchemeActionHandler() {
        return new GetOrganisationsFromSchemeActionHandler();
    }

    @Bean
    public FindAllCommonMetadataActionHandler getfindAllCommonMetadataActionHandler() {
        return new FindAllCommonMetadataActionHandler();
    }

    @Bean
    public FindAllConceptSchemesActionHandler getFindAllConceptSchemesActionHandler() {
        return new FindAllConceptSchemesActionHandler();
    }

    @Bean
    public GetConceptsFromSchemeActionHandler getConceptsFromSchemeActionHandler() {
        return new GetConceptsFromSchemeActionHandler();
    }

    @Bean
    public FindAllCodeListsActionHandler getFindAllCodeListsActionHandler() {
        return new FindAllCodeListsActionHandler();
    }

    @Bean
    public GetCodesFromCodeListActionHandler getCodesFromCodeListActionHandler() {
        return new GetCodesFromCodeListActionHandler();
    }

    @Bean
    public GetFrequencyCodesActionHandler getFrequencyCodesActionHandler() {
        return new GetFrequencyCodesActionHandler();
    }

    @Bean
    public UpdateInstancesOrderActionHandler getUpdateInstancesOrderActionHandler() {
        return new UpdateInstancesOrderActionHandler();
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
        bindHandler(GetGopestatListsAction.class, GetGopestatListsActionHandler.class);
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
    }

}
