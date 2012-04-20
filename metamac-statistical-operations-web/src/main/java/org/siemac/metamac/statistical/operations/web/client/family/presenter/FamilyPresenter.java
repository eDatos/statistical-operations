package org.siemac.metamac.statistical.operations.web.client.family.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsResult;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyFamilyResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyResult;
import org.siemac.metamac.statistical.operations.web.shared.UpdateFamilyOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateFamilyOperationsResult;
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

public class FamilyPresenter extends Presenter<FamilyPresenter.FamilyView, FamilyPresenter.FamilyProxy> implements FamilyUiHandlers {

    private final DispatchAsync                dispatcher;
    private final PlaceManager                 placeManager;

    private Long                               idFamily;
    private FamilyDto                          familyDto;
    private List<OperationBaseDto>             operationBaseDtos;

    private OperationsToolStripPresenterWidget operationsToolStripPresenterWidget;

    public static final Object                 TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.familyPage)
    public interface FamilyProxy extends Proxy<FamilyPresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbStatisticalFamily();
    }

    public interface FamilyView extends View, HasUiHandlers<FamilyUiHandlers> {

        // Family
        void setFamily(FamilyDto familyDto, List<OperationBaseDto> operationBaseDtos);
        FamilyDto getFamily(FamilyDto familyDto);
        HasClickHandlers getSave();
        void onFamilySaved(FamilyDto familyDto);
        HasClickHandlers getPublishFamilyInternally();
        HasClickHandlers getPublishFamilyExternally();
        boolean validate();
        // Operations
        HasRecordClickHandlers getSelectedOperation();
        void setOperations(List<OperationBaseDto> operationBaseDtos);
        void setAllOperations(List<OperationBaseDto> operationBaseDtos);
        com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getAddOperations();
        List<Long> getSelectedOperationIds();
        boolean validateAddOperations();
        void closeOperationsWindow();
    }

    @Inject
    public FamilyPresenter(EventBus eventBus, FamilyView familyView, FamilyProxy familyProxy, DispatchAsync dispatcher, PlaceManager placeManager,
            OperationsToolStripPresenterWidget operationsToolStripPresenterWidget) {
        super(eventBus, familyView, familyProxy);
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

        registerHandler(getView().getSelectedOperation().addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                OperationRecord record = (OperationRecord) event.getRecord();
                goToOperation(record.getId());
            }
        }));

        registerHandler(getView().getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (getView().validate()) {
                    saveFamily(getView().getFamily(familyDto));
                }
            }
        }));

        registerHandler(getView().getAddOperations().addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                if (getView().validateAddOperations()) {
                    List<Long> selectedOperationIds = getView().getSelectedOperationIds();
                    List<Long> familyOperationsId = getOperationIds(operationBaseDtos);
                    // Operations to add
                    List<Long> operationsToAdd = new ArrayList<Long>();
                    for (Long id : selectedOperationIds) {
                        if (!familyOperationsId.contains(id)) {
                            operationsToAdd.add(id);
                        }
                    }
                    // Operations to remove
                    List<Long> operationsToRemove = new ArrayList<Long>();
                    for (OperationBaseDto operationBaseDto : operationBaseDtos) {
                        if (!selectedOperationIds.contains(operationBaseDto.getId())) {
                            operationsToRemove.add(operationBaseDto.getId());
                        }
                    }
                    if (!operationsToAdd.isEmpty() || !operationsToRemove.isEmpty()) {
                        updateFamilyOperations(operationsToAdd, operationsToRemove);
                    } else {
                        getView().closeOperationsWindow();
                        ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getMessageList(getMessages().operationAddedToFamily()), MessageTypeEnum.SUCCESS);
                    }
                }
            }
        }));

        registerHandler(getView().getPublishFamilyInternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // TODO Validate
                publishFamilyInternally();
            }
        }));

        registerHandler(getView().getPublishFamilyExternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // TODO Validate
                publishFamilyExternally();
            }
        }));
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().statisticalFamily());
        setInSlot(TYPE_SetContextAreaContentToolBar, operationsToolStripPresenterWidget);
    }

    @Override
    public void prepareFromRequest(PlaceRequest request) {
        super.prepareFromRequest(request);
        String id = request.getParameter(PlaceRequestParams.familyParam, null);
        if (id != null) {
            idFamily = Long.valueOf(id);
            retrieveFamily(idFamily);
        }
    }

    private void retrieveFamily(Long idFamily) {
        dispatcher.execute(new GetFamilyAndOperationsAction(idFamily), new AsyncCallback<GetFamilyAndOperationsResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetFamilyAndOperationsResult result) {
                familyDto = result.getFamilyDto();
                operationBaseDtos = result.getOperationBaseDtos();
                MainPagePresenter.getMasterHead().setTitleLabel(getMessages().titleStatisticalFamily(familyDto.getCode()));
                dispatcher.execute(new GetOperationListAction(), new AsyncCallback<GetOperationListResult>() {

                    @Override
                    public void onFailure(Throwable caught) {
                        ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
                    }
                    @Override
                    public void onSuccess(GetOperationListResult result) {
                        // Make sure the operation list is set before setting family operations
                        getView().setAllOperations(result.getOperationBaseDtos());
                        getView().setFamily(familyDto, operationBaseDtos);
                    }
                });
            }
        });
    }

    @Override
    public void saveFamily(FamilyDto familyToSave) {
        dispatcher.execute(new SaveFamilyAction(familyToSave), new AsyncCallback<SaveFamilyResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(SaveFamilyResult result) {
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getMessageList(getMessages().familySaved()), MessageTypeEnum.SUCCESS);
                familyDto = result.getFamilySaved();
                getView().onFamilySaved(familyDto);
            }
        });
    }

    @Override
    public void goToOperation(Long idOperation) {
        if (idOperation != null) {
            placeManager.revealRelativePlace(new PlaceRequest(NameTokens.operationPage).with(PlaceRequestParams.operationParam, idOperation.toString()));
        }
    }

    @Override
    public void updateFamilyOperations(List<Long> operationsToAdd, List<Long> operationsToRemove) {
        dispatcher.execute(new UpdateFamilyOperationsAction(idFamily, operationsToAdd, operationsToRemove), new AsyncCallback<UpdateFamilyOperationsResult>() {

            @Override
            public void onFailure(Throwable caught) {
                getView().closeOperationsWindow();
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorAddToFamily()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(UpdateFamilyOperationsResult result) {
                operationBaseDtos = result.getOperationDtos();
                getView().setOperations(operationBaseDtos);
                getView().closeOperationsWindow();
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getMessageList(getMessages().operationsAddedToFamily()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private List<Long> getOperationIds(List<OperationBaseDto> operationBaseDtos) {
        List<Long> operationIds = new ArrayList<Long>();
        for (OperationBaseDto operationDto : operationBaseDtos) {
            operationIds.add(operationDto.getId());
        }
        return operationIds;
    }

    private void publishFamilyInternally() {
        dispatcher.execute(new PublishInternallyFamilyAction(familyDto.getId()), new AsyncCallback<PublishInternallyFamilyResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorInternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(PublishInternallyFamilyResult result) {
                familyDto = result.getFamilySaved();
                getView().onFamilySaved(familyDto);
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getMessageList(getMessages().familyInternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private void publishFamilyExternally() {
        dispatcher.execute(new PublishExternallyFamilyAction(familyDto.getId()), new AsyncCallback<PublishExternallyFamilyResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorExternallyPublishing()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(PublishExternallyFamilyResult result) {
                familyDto = result.getFamilySaved();
                getView().onFamilySaved(familyDto);
                ShowMessageEvent.fire(FamilyPresenter.this, ErrorUtils.getMessageList(getMessages().familyExternallyPublished()), MessageTypeEnum.SUCCESS);
            }
        });
    }

}
