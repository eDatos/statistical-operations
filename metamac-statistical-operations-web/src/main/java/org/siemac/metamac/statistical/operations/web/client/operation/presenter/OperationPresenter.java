package org.siemac.metamac.statistical.operations.web.client.operation.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateCategorySchemesEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateCategorySchemesEvent.UpdateCategorySchemesHandler;
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
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeResult;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListResult;
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
import org.siemac.metamac.web.common.client.enums.MessageTypeEnum;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;

import com.google.gwt.event.shared.EventBus;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.inject.Inject;
import com.gwtplatform.dispatch.shared.DispatchAsync;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.ProxyEvent;
import com.gwtplatform.mvp.client.annotations.TitleFunction;
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
            UpdateCategorySchemesHandler,
            UpdateOrganisationSchemesHandler,
            UpdateOperationsListsHandler,
            UpdateCommonMetadataHandler,
            UpdateFrequencyCodesHandler {

    private final DispatchAsync              dispatcher;
    private final PlaceManager               placeManager;

    private Long                             idOperation;
    private OperationDto                     operationDto;
    private List<InstanceBaseDto>            instanceBaseDtos;
    private List<FamilyBaseDto>              familyBaseDtos;

    private OperationsToolStripPresenterWidget operationsToolStripPresenterWidget;

    public static final Object               TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.operationPage)
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
        HasRecordClickHandlers getSelectedFamily();
        HasClickHandlers getSave();
        void onOperationSaved(OperationDto operationDto);
        boolean validate();
        HasClickHandlers getPublishOperationInternally();
        HasClickHandlers getPublishOperationExternally();

        void setCategorySchemes(List<ExternalItemBtDto> schemes);
        void setSubjects(List<ExternalItemBtDto> subjects);
        void setSecondarySubjetcs(List<ExternalItemBtDto> secondarySubjects);

        void setOrganisationSchemes(List<ExternalItemBtDto> schemes);
        void setProducers(List<ExternalItemBtDto> organisations);
        void setRegionalResposibles(List<ExternalItemBtDto> organisations);
        void setRegionalContributors(List<ExternalItemBtDto> organisations);
        void setPublishers(List<ExternalItemBtDto> organisations);
        void setUpdateFrequencyCodes(List<ExternalItemBtDto> codes);

        void setCommonMetadataList(List<ExternalItemBtDto> commonMetadataList);

        void setOperationsLists(List<SurveyTypeDto> surveyTypeDtos, List<OfficialityTypeDto> officialityTypeDtos);

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
        void setFamilies(List<FamilyBaseDto> familyBaseDtos);
        com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getAddFamilies();
        List<Long> getSelectedFamilyIds();
        boolean validateAddFamilies();
        void closeFamiliesWindow();
        void setAllFamilies(List<FamilyBaseDto> familyBaseDtos);
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
                goToFamily(record.getId());
            }
        }));

        registerHandler(getView().getSelectedInstance().addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                if (event.getFieldNum() != 0) {
                    InstanceRecord record = (InstanceRecord) event.getRecord();
                    goToInstance(record.getId());
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

        registerHandler(getView().getAddFamilies().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                if (getView().validateAddFamilies()) {
                    List<Long> selectedFamilyIds = getView().getSelectedFamilyIds();
                    List<Long> operationFamiliesId = getFamilyIds(familyBaseDtos);
                    // Families to add
                    List<Long> familiesToAdd = new ArrayList<Long>();
                    for (Long id : selectedFamilyIds) {
                        if (!operationFamiliesId.contains(id)) {
                            familiesToAdd.add(id);
                        }
                    }
                    // Families to remove
                    List<Long> familiesToRemove = new ArrayList<Long>();
                    for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
                        if (!selectedFamilyIds.contains(familyBaseDto.getId())) {
                            familiesToRemove.add(familyBaseDto.getId());
                        }
                    }
                    if (!familiesToAdd.isEmpty() || !familiesToRemove.isEmpty()) {
                        updateOperationFamilies(familiesToAdd, familiesToRemove);
                    } else {
                        getView().closeFamiliesWindow();
                        ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().familiesAddedToOperation()), MessageTypeEnum.SUCCESS);
                    }
                }
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
        String id = request.getParameter(PlaceRequestParams.operationParam, null);
        if (id != null) {
            idOperation = Long.valueOf(id);
            retrieveOperation(idOperation);
        }
    }

    @Override
    public void saveOperation(OperationDto operationToSave) {
        dispatcher.execute(new SaveOperationAction(operationToSave), new AsyncCallback<SaveOperationResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(SaveOperationResult result) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().operationSaved()), MessageTypeEnum.SUCCESS);
                operationDto = result.getOperationSaved();
                getView().onOperationSaved(operationDto);
            }
        });
    }

    @Override
    public void goToFamily(Long idFamily) {
        if (idFamily != null) {
            PlaceRequest familyListRequest = new PlaceRequest(NameTokens.familyListPage);
            PlaceRequest familyRequest = new PlaceRequest(NameTokens.familyPage).with(PlaceRequestParams.familyParam, idFamily.toString());
            List<PlaceRequest> placeRequestHierarchy = new ArrayList<PlaceRequest>();
            placeRequestHierarchy.add(familyListRequest);
            placeRequestHierarchy.add(familyRequest);
            placeManager.revealPlaceHierarchy(placeRequestHierarchy);
        }
    }

    @Override
    public void saveInstance(InstanceDto instanceDto) {
        dispatcher.execute(new SaveInstanceAction(idOperation, instanceDto), new AsyncCallback<SaveInstanceResult>() {

            @Override
            public void onFailure(Throwable caught) {
                getView().closeInstanceWindow();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(SaveInstanceResult result) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().instanceSaved()), MessageTypeEnum.SUCCESS);
                getView().closeInstanceWindow();
                retrieveOperation(idOperation);
            }
        });
    }

    @Override
    public void goToInstance(Long idInstance) {
        if (idInstance != null) {
            placeManager.revealRelativePlace(new PlaceRequest(NameTokens.instancePage).with(PlaceRequestParams.instanceParam, idInstance.toString()));
        }
    }

    @Override
    public void deleteInstances(final List<Long> instanceIds) {
        dispatcher.execute(new DeleteInstanceListAction(instanceIds), new AsyncCallback<DeleteInstanceListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                retrieveInstances();
                String message = instanceIds.size() > 1 ? getMessages().instanceErrorDelete() : getMessages().instancesErrorDelete();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, message), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(DeleteInstanceListResult result) {
                retrieveOperation(idOperation);
                String message = instanceIds.size() > 1 ? getMessages().instancesDeleted() : getMessages().instanceDeleted();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(message), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @Override
    public void updateOperationFamilies(List<Long> familiesToAdd, List<Long> familiesToRemove) {
        dispatcher.execute(new UpdateOperationFamiliesAction(idOperation, familiesToAdd, familiesToRemove), new AsyncCallback<UpdateOperationFamiliesResult>() {

            @Override
            public void onFailure(Throwable caught) {
                getView().closeFamiliesWindow();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorAddToOperation()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(UpdateOperationFamiliesResult result) {
                retrieveOperation(idOperation);
                getView().closeFamiliesWindow();
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().familiesAddedToOperation()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateCategorySchemes(UpdateCategorySchemesEvent event) {
        getView().setCategorySchemes(event.getCategorySchemes());
    }

    private void retrieveOperation(Long idOperation) {
        dispatcher.execute(new GetOperationAndInstancesAction(idOperation), new AsyncCallback<GetOperationAndInstancesResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetOperationAndInstancesResult result) {
                operationDto = result.getOperationDto();
                instanceBaseDtos = result.getInstanceBaseDtos();
                familyBaseDtos = result.getFamilyBaseDtos();
                MainPagePresenter.getMasterHead().setTitleLabel(getMessages().titleStatisticalOperation(operationDto.getCode()));
                dispatcher.execute(new GetFamilyListAction(), new AsyncCallback<GetFamilyListResult>() {

                    @Override
                    public void onFailure(Throwable caught) {
                        ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familiesErrorRetrievingData()), MessageTypeEnum.ERROR);
                    }
                    @Override
                    public void onSuccess(GetFamilyListResult result) {
                        // Make sure the family list is set before setting operation families
                        getView().setAllFamilies(result.getFamilyBaseDtos());
                        getView().setOperation(operationDto, instanceBaseDtos, familyBaseDtos);
                    }
                });
            }
        });
    }

    private void retrieveInstances() {
        dispatcher.execute(new GetInstanceListAction(idOperation), new AsyncCallback<GetInstanceListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instancesErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetInstanceListResult result) {
                getView().setInstances(result.getInstanceBaseDtos());
            }
        });
    }

    private List<Long> getFamilyIds(List<FamilyBaseDto> familyBaseDtos) {
        List<Long> familyIds = new ArrayList<Long>();
        for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
            familyIds.add(familyBaseDto.getId());
        }
        return familyIds;
    }

    private void publishOperationInternally() {
        dispatcher.execute(new PublishInternallyOperationAction(operationDto.getId()), new AsyncCallback<PublishInternallyOperationResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorInternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(PublishInternallyOperationResult result) {
                operationDto = result.getOperationSaved();
                getView().onOperationSaved(operationDto);
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().operationInternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private void publishOperationExternally() {
        dispatcher.execute(new PublishExternallyOperationAction(operationDto.getId()), new AsyncCallback<PublishExternallyOperationResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorExternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(PublishExternallyOperationResult result) {
                operationDto = result.getOperationSaved();
                getView().onOperationSaved(operationDto);
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getMessageList(getMessages().operationExternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @Override
    public void populateSubjects(String uri) {
        dispatcher.execute(new GetCategoriesFromSchemeAction(uri), new AsyncCallback<GetCategoriesFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetCategoriesFromSchemeResult result) {
                getView().setSubjects(result.getCategories());
            }
        });
    }

    @Override
    public void populateSecondarySubjects(String scehemUri) {
        dispatcher.execute(new GetCategoriesFromSchemeAction(scehemUri), new AsyncCallback<GetCategoriesFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetCategoriesFromSchemeResult result) {
                getView().setSecondarySubjetcs(result.getCategories());
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateOperationsLists(UpdateOperationsListsEvent event) {
        getView().setOperationsLists(event.getSurveyTypeDtos(), event.getOfficialityTypeDtos());
    }

    @Override
    public void populateProducers(String uri) {
        dispatcher.execute(new GetOrganisationsFromSchemeAction(uri), new AsyncCallback<GetOrganisationsFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetOrganisationsFromSchemeResult result) {
                getView().setProducers(result.getOrganisations());
            }
        });
    }

    @Override
    public void populateRegionalResposibles(String uri) {
        dispatcher.execute(new GetOrganisationsFromSchemeAction(uri), new AsyncCallback<GetOrganisationsFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetOrganisationsFromSchemeResult result) {
                getView().setRegionalResposibles(result.getOrganisations());
            }
        });
    }

    @Override
    public void populateRegionalContributors(String uri) {
        dispatcher.execute(new GetOrganisationsFromSchemeAction(uri), new AsyncCallback<GetOrganisationsFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetOrganisationsFromSchemeResult result) {
                getView().setRegionalContributors(result.getOrganisations());
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateOrganisationSchemes(UpdateOrganisationSchemesEvent event) {
        getView().setOrganisationSchemes(event.getOrganisationSchemes());
    }

    @Override
    public void populatePublishers(String uri) {
        dispatcher.execute(new GetOrganisationsFromSchemeAction(uri), new AsyncCallback<GetOrganisationsFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetOrganisationsFromSchemeResult result) {
                getView().setPublishers(result.getOrganisations());
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateCommonMetadata(UpdateCommonMetadataEvent event) {
        getView().setCommonMetadataList(event.getCommonMetadata());
    }

    @ProxyEvent
    @Override
    public void onUpdateFrequencyCodes(UpdateFrequencyCodesEvent event) {
        getView().setUpdateFrequencyCodes(event.getUpdateFrequencyCodes());
    }

    @Override
    public void updateInstancesOrder(List<Long> instancesIds) {
        dispatcher.execute(new UpdateInstancesOrderAction(idOperation, instancesIds), new AsyncCallback<UpdateInstancesOrderResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().instanceErrorUpdatingOrder()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(UpdateInstancesOrderResult result) {
                getView().setInstances(result.getInstances());
            }
        });
    }

}
