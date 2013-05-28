package org.siemac.metamac.statistical.operations.web.client.operation.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.constants.shared.UrnConstants;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.core.common.util.shared.UrnUtils;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.navigation.shared.NameTokens;
import org.siemac.metamac.statistical.operations.navigation.shared.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.LoggedInGatekeeper;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateCommonMetadataEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateCommonMetadataEvent.UpdateCommonMetadataHandler;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateFrequencyCodesEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateFrequencyCodesEvent.UpdateFrequencyCodesHandler;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOperationsListsEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOperationsListsEvent.UpdateOperationsListsHandler;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOrganisationSchemesEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOrganisationSchemesEvent.UpdateOrganisationSchemesHandler;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.PlaceRequestUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetCommonMetadataConfigurationsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCommonMetadataConfigurationsResult;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyPaginatedListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyPaginatedListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesResult;
import org.siemac.metamac.statistical.operations.web.shared.GetOrganisationsFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOrganisationsFromSchemeResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationResult;
import org.siemac.metamac.statistical.operations.web.shared.UpdateInstancesOrderAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateInstancesOrderResult;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesResult;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesResult;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.web.common.client.enums.MessageTypeEnum;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.google.gwt.event.shared.EventBus;
import com.google.inject.Inject;
import com.gwtplatform.dispatch.shared.DispatchAsync;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.ProxyEvent;
import com.gwtplatform.mvp.client.annotations.TitleFunction;
import com.gwtplatform.mvp.client.annotations.UseGatekeeper;
import com.gwtplatform.mvp.client.proxy.Place;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.gwtplatform.mvp.client.proxy.Proxy;
import com.gwtplatform.mvp.client.proxy.RevealContentEvent;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class OperationPresenter extends Presenter<OperationPresenter.OperationView, OperationPresenter.OperationProxy>
        implements
            OperationUiHandlers,
            UpdateOrganisationSchemesHandler,
            UpdateOperationsListsHandler,
            UpdateCommonMetadataHandler,
            UpdateFrequencyCodesHandler {

    private final DispatchAsync                dispatcher;
    private final PlaceManager                 placeManager;

    private OperationDto                       operationDto;
    private List<InstanceBaseDto>              instanceBaseDtos;
    private List<FamilyBaseDto>                familyBaseDtos;

    private OperationsToolStripPresenterWidget operationsToolStripPresenterWidget;

    public static final Object                 TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.operationPage)
    @UseGatekeeper(LoggedInGatekeeper.class)
    public interface OperationProxy extends Proxy<OperationPresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbStatisticalOperation();
    }

    public interface OperationView extends View, HasUiHandlers<OperationUiHandlers> {

        // Operations

        void setOperation(OperationDto operationDto, List<InstanceBaseDto> instanceBaseDtos, List<FamilyBaseDto> familyBaseDtos);
        OperationDto getOperation(OperationDto operationDto);
        HasClickHandlers getSave();
        void onOperationSaved(OperationDto operationDto);
        boolean validate();
        HasClickHandlers getPublishOperationInternally();
        HasClickHandlers getPublishOperationExternally();

        void setOperationsLists(List<SurveyTypeDto> surveyTypeDtos, List<OfficialityTypeDto> officialityTypeDtos);

        void setOrganisationSchemes(List<ExternalItemDto> schemes);
        void setRegionalContributors(List<ExternalItemDto> organisations);
        void setUpdateFrequencyCodes(List<ExternalItemDto> codes);

        // External resources

        void setCommonMetadataConfigurations(List<ExternalItemDto> commonMetadataList);
        void setItemSchemes(String formItemName, ExternalItemsResult result);
        void setItems(String formItemName, ExternalItemsResult result);

        // Instances

        HasRecordClickHandlers getSelectedInstance();
        void setInstances(List<InstanceBaseDto> instanceBaseDtos);
        com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getSaveNewInstance();
        InstanceDto getNewInstace();
        void onInstanceSaved(InstanceDto instanceDto);
        boolean validateNewInstance();
        void closeInstanceWindow();
        HasClickHandlers getDeleteInstance();
        List<Long> getSelectedInstances();

        // Families
        HasRecordClickHandlers getSelectedFamily();
        void setFamilies(List<FamilyBaseDto> familyBaseDtos, int firstResult, int totalResults);
    }

    @Inject
    public OperationPresenter(EventBus eventBus, OperationView operationView, OperationProxy operationProxy, DispatchAsync dispatcher, PlaceManager placeManager,
            OperationsToolStripPresenterWidget operationsToolStripPresenterWidget) {
        super(eventBus, operationView, operationProxy);
        this.dispatcher = dispatcher;
        this.placeManager = placeManager;
        this.operationsToolStripPresenterWidget = operationsToolStripPresenterWidget;
        getView().setUiHandlers(this);
    }

    @Override
    protected void revealInParent() {
        RevealContentEvent.fire(this, MainPagePresenter.TYPE_SetContextAreaContent, this);
    }

    @Override
    protected void onBind() {
        super.onBind();

        registerHandler(getView().getSelectedFamily().addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                FamilyRecord record = (FamilyRecord) event.getRecord();
                goToFamily(record.getCode());
            }
        }));

        registerHandler(getView().getSelectedInstance().addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                if (event.getFieldNum() != 0) {
                    InstanceRecord record = (InstanceRecord) event.getRecord();
                    goToInstance(record.getCode());
                }
            }
        }));

        registerHandler(getView().getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (getView().validate()) {
                    saveOperation(getView().getOperation(operationDto));
                }
            }
        }));

        registerHandler(getView().getSaveNewInstance().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                if (getView().validateNewInstance()) {
                    saveInstance(getView().getNewInstace());
                }
            }
        }));

        registerHandler(getView().getDeleteInstance().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                deleteInstances(getView().getSelectedInstances());
            }
        }));

        registerHandler(getView().getPublishOperationInternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // TODO Validate
                publishOperationInternally();
            }
        }));

        registerHandler(getView().getPublishOperationExternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // TODO Validate
                publishOperationExternally();
            }
        }));
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().statisticalOperation());
        setInSlot(TYPE_SetContextAreaContentToolBar, operationsToolStripPresenterWidget);
    }

    @Override
    public void prepareFromRequest(PlaceRequest request) {
        super.prepareFromRequest(request);
        String operationCode = request.getParameter(PlaceRequestParams.operationParam, null);
        if (!StringUtils.isBlank(operationCode)) {
            retrieveOperation(operationCode);
        } else {
            OperationsWeb.showErrorPage();
        }
    }

    @Override
    public void saveOperation(OperationDto operationToSave) {
        dispatcher.execute(new SaveOperationAction(operationToSave), new WaitingAsyncCallback<SaveOperationResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(SaveOperationResult result) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().operationSaved()), MessageTypeEnum.SUCCESS);
                operationDto = result.getOperationSaved();
                getView().onOperationSaved(operationDto);

                // Update URL
                PlaceRequest placeRequest = PlaceRequestUtils.buildRelativeOperationPlaceRequest(operationDto.getCode());
                placeManager.updateHistory(placeRequest, true);
            }
        });
    }

    @Override
    public void goToFamily(String familyCode) {
        if (!StringUtils.isBlank(familyCode)) {
            PlaceRequest familyListRequest = new PlaceRequest(NameTokens.familyListPage);
            PlaceRequest familyRequest = PlaceRequestUtils.buildRelativeFamilyPlaceRequest(familyCode);
            List<PlaceRequest> placeRequestHierarchy = new ArrayList<PlaceRequest>();
            placeRequestHierarchy.add(familyListRequest);
            placeRequestHierarchy.add(familyRequest);
            placeManager.revealPlaceHierarchy(placeRequestHierarchy);
        }
    }

    @Override
    public void saveInstance(InstanceDto instanceDto) {
        dispatcher.execute(new SaveInstanceAction(operationDto.getId(), instanceDto), new WaitingAsyncCallback<SaveInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                getView().closeInstanceWindow();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(SaveInstanceResult result) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().instanceSaved()), MessageTypeEnum.SUCCESS);
                getView().closeInstanceWindow();
                retrieveOperation(operationDto.getCode());
            }
        });
    }

    @Override
    public void goToInstance(String instanceCode) {
        if (!StringUtils.isBlank(instanceCode)) {
            placeManager.revealRelativePlace(PlaceRequestUtils.buildRelativeInstancePlaceRequest(instanceCode));
        }
    }

    @Override
    public void deleteInstances(final List<Long> instanceIds) {
        dispatcher.execute(new DeleteInstanceListAction(instanceIds), new WaitingAsyncCallback<DeleteInstanceListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                retrieveInstances();
                String message = instanceIds.size() > 1 ? getMessages().instanceErrorDelete() : getMessages().instancesErrorDelete();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, message), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(DeleteInstanceListResult result) {
                retrieveOperation(operationDto.getCode());
                String message = instanceIds.size() > 1 ? getMessages().instancesDeleted() : getMessages().instanceDeleted();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(message), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @Override
    public void updateOperationFamilies(List<Long> familiesToAdd, List<Long> familiesToRemove) {
        dispatcher.execute(new UpdateOperationFamiliesAction(operationDto.getId(), familiesToAdd, familiesToRemove), new WaitingAsyncCallback<UpdateOperationFamiliesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorAddToOperation()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(UpdateOperationFamiliesResult result) {
                retrieveOperation(operationDto.getCode());
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().familiesAddedToOperation()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private void retrieveOperation(String operationCode) {
        String operationUrn = UrnUtils.generateUrn(UrnConstants.URN_SIEMAC_CLASS_OPERATION_PREFIX, operationCode);
        dispatcher.execute(new GetOperationAndInstancesAction(operationUrn), new WaitingAsyncCallback<GetOperationAndInstancesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorRetrievingData()), MessageTypeEnum.ERROR);
                OperationsWeb.showErrorPage();
            }
            @Override
            public void onWaitSuccess(GetOperationAndInstancesResult result) {
                operationDto = result.getOperationDto();
                instanceBaseDtos = result.getInstanceBaseDtos();
                familyBaseDtos = result.getFamilyBaseDtos();
                MainPagePresenter.getMasterHead().setTitleLabel(getMessages().titleStatisticalOperation(operationDto.getCode()));
                getView().setOperation(operationDto, instanceBaseDtos, familyBaseDtos);
            }
        });
    }

    private void retrieveInstances() {
        dispatcher.execute(new GetInstanceListAction(operationDto.getId()), new WaitingAsyncCallback<GetInstanceListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instancesErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetInstanceListResult result) {
                getView().setInstances(result.getInstanceBaseDtos());
            }
        });
    }

    private void publishOperationInternally() {
        dispatcher.execute(new PublishInternallyOperationAction(operationDto.getId()), new WaitingAsyncCallback<PublishInternallyOperationResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorInternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(PublishInternallyOperationResult result) {
                retrieveOperation(operationDto.getCode());
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().operationInternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private void publishOperationExternally() {
        dispatcher.execute(new PublishExternallyOperationAction(operationDto.getId()), new WaitingAsyncCallback<PublishExternallyOperationResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorExternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(PublishExternallyOperationResult result) {
                retrieveOperation(operationDto.getCode());
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().operationExternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateOperationsLists(UpdateOperationsListsEvent event) {
        getView().setOperationsLists(event.getSurveyTypeDtos(), event.getOfficialityTypeDtos());
    }

    @Override
    public void populateRegionalContributors(String uri) {
        dispatcher.execute(new GetOrganisationsFromSchemeAction(uri), new WaitingAsyncCallback<GetOrganisationsFromSchemeResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetOrganisationsFromSchemeResult result) {
                getView().setRegionalContributors(result.getOrganisations());
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateOrganisationSchemes(UpdateOrganisationSchemesEvent event) {
        getView().setOrganisationSchemes(event.getOrganisationSchemes());
    }

    @ProxyEvent
    @Override
    public void onUpdateCommonMetadata(UpdateCommonMetadataEvent event) {
        getView().setCommonMetadataConfigurations(event.getCommonMetadata());
    }

    @ProxyEvent
    @Override
    public void onUpdateFrequencyCodes(UpdateFrequencyCodesEvent event) {
        getView().setUpdateFrequencyCodes(event.getUpdateFrequencyCodes());
    }

    @Override
    public void updateInstancesOrder(List<Long> instancesIds) {
        dispatcher.execute(new UpdateInstancesOrderAction(operationDto.getId(), instancesIds), new WaitingAsyncCallback<UpdateInstancesOrderResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorUpdatingOrder()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(UpdateInstancesOrderResult result) {
                getView().setInstances(result.getInstances());
            }
        });
    }

    @Override
    public void retrievePaginatedFamilies(int firstResult, int maxResults, String family) {
        dispatcher.execute(new GetFamilyPaginatedListAction(firstResult, maxResults, family), new WaitingAsyncCallback<GetFamilyPaginatedListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familiesErrorRetrievingData()), MessageTypeEnum.ERROR);
            }

            @Override
            public void onWaitSuccess(GetFamilyPaginatedListResult result) {
                getView().setFamilies(result.getFamilyBaseDtos(), result.getFirstResultOut(), result.getTotalResults());
            }
        });
    }

    //
    // EXTERNAL RESOURCES
    //

    @Override
    public void retrieveCommonMetadataConfigurations() {
        dispatcher.execute(new GetCommonMetadataConfigurationsAction(null), new WaitingAsyncCallback<GetCommonMetadataConfigurationsResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorRetrievingConfigurations()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetCommonMetadataConfigurationsResult result) {
                getView().setCommonMetadataConfigurations(result.getConfigurations());
            }
        });
    }

    @Override
    public void retrieveItemSchemes(final String formItemName, TypeExternalArtefactsEnum type, int firstResult, int maxResults, String criteria) {
        ExternalResourceWebCriteria externalResourceWebCriteria = new ExternalResourceWebCriteria(type, criteria);
        dispatcher.execute(new GetExternalResourcesAction(externalResourceWebCriteria, firstResult, maxResults), new WaitingAsyncCallback<GetExternalResourcesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().errorRetrievingExternalStructuralResources()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetExternalResourcesResult result) {
                getView().setItemSchemes(formItemName, result.getExternalItemsResult());
            }
        });
    }

    @Override
    public void retrieveItems(final String formItemName, TypeExternalArtefactsEnum type, int firstResult, int maxResults, String criteria, String categorySchemeUrn) {
        ItemWebCriteria itemWebCriteria = new ItemWebCriteria(type, criteria);
        itemWebCriteria.setItemSchemUrn(categorySchemeUrn);
        dispatcher.execute(new GetExternalResourcesAction(itemWebCriteria, firstResult, maxResults), new WaitingAsyncCallback<GetExternalResourcesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().errorRetrievingExternalStructuralResources()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetExternalResourcesResult result) {
                getView().setItems(formItemName, result.getExternalItemsResult());
            }
        });
    }

    //
    // NAVIGATION
    //

    @Override
    public void goTo(List<PlaceRequest> location) {
        if (location != null && !location.isEmpty()) {
            placeManager.revealPlaceHierarchy(location);
        }
    }
}
