package org.siemac.metamac.statistical.operations.web.client.operation.presenter;

import static org.siemac.metamac.statistical.operations.web.client.GopestatWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.GopestatWeb.getMessages;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.GopestatToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListResult;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesResult;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeResult;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationResult;
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
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class OperationListPresenter extends Presenter<OperationListPresenter.OperationListView, OperationListPresenter.OperationsListProxy> implements OperationListUiHandlers {

    private final DispatchAsync              dispatcher;
    private final PlaceManager               placeManager;

    private GopestatToolStripPresenterWidget gopestatToolStripPresenterWidget;

    public static final Object               TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.operationListPage)
    public interface OperationsListProxy extends Proxy<OperationListPresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbStatisticalOperations();
    }

    public interface OperationListView extends View, HasUiHandlers<OperationListUiHandlers> {

        void setOperations(List<OperationBaseDto> operationBaseDtos);
        HasRecordClickHandlers getSelectedOperation();
        HasClickHandlers getSaveNewOperation();
        OperationDto getOperation();
        boolean validate();
        void closeOperationWindow();
        com.smartgwt.client.widgets.events.HasClickHandlers getDelete();
        List<Long> getSelectedOperations();
        void onOperationSaved(OperationDto operationDto);

        void setCategorySchemes(List<ExternalItemBtDto> schemes);
        void setSubjects(List<ExternalItemBtDto> subjects);
    }

    @Inject
    public OperationListPresenter(EventBus eventBus, OperationListView operationListView, OperationsListProxy operationsListProxy, DispatchAsync dispatcher, PlaceManager placeManager,
            GopestatToolStripPresenterWidget gopestatToolStripPresenterWidget) {
        super(eventBus, operationListView, operationsListProxy);
        this.dispatcher = dispatcher;
        this.placeManager = placeManager;
        this.gopestatToolStripPresenterWidget = gopestatToolStripPresenterWidget;
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
                if (event.getFieldNum() != 0) {
                    OperationRecord record = (OperationRecord) event.getRecord();
                    goToOperation(record.getId());
                }
            }
        }));

        registerHandler(getView().getSaveNewOperation().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (getView().validate()) {
                    saveOperation(getView().getOperation());
                }
            }
        }));

        registerHandler(getView().getDelete().addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                deleteOperations(getView().getSelectedOperations());
            }
        }));
    }

    @Override
    protected void onReset() {
        super.onReset();
        retrieveOperations();
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().statisticalOperations());
        setInSlot(TYPE_SetContextAreaContentToolBar, gopestatToolStripPresenterWidget);
    }

    @Override
    public void goToOperation(Long idOperation) {
        if (idOperation != null) {
            placeManager.revealRelativePlace(new PlaceRequest(NameTokens.operationPage).with(PlaceRequestParams.operationParam, idOperation.toString()));
        }
    }

    @Override
    public void saveOperation(OperationDto operationToSave) {
        dispatcher.execute(new SaveOperationAction(operationToSave), new AsyncCallback<SaveOperationResult>() {

            @Override
            public void onFailure(Throwable caught) {
                getView().closeOperationWindow();
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(SaveOperationResult result) {
                getView().closeOperationWindow();
                getView().onOperationSaved(result.getOperationSaved());
            }
        });
    }

    private void retrieveOperations() {
        dispatcher.execute(new GetOperationListAction(), new AsyncCallback<GetOperationListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetOperationListResult result) {
                getView().setOperations(result.getOperationBaseDtos());
            }
        });
    }

    @Override
    public void deleteOperations(List<Long> operationIds) {
        dispatcher.execute(new DeleteOperationListAction(operationIds), new AsyncCallback<DeleteOperationListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                retrieveOperations();
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorDelete()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(DeleteOperationListResult result) {
                retrieveOperations();
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getMessageList(getMessages().operationDeleted()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @Override
    public void populateSubjects(String uri) {
        dispatcher.execute(new GetCategoriesFromSchemeAction(uri), new AsyncCallback<GetCategoriesFromSchemeResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetCategoriesFromSchemeResult result) {
                getView().setSubjects(result.getCategories());
            }
        });
    }

    @Override
    public void retrieveCategorySchemes() {
        dispatcher.execute(new FindAllCategorySchemesAction(), new AsyncCallback<FindAllCategorySchemesResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().listsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(FindAllCategorySchemesResult result) {
                getView().setCategorySchemes(result.getCategorySchemes());
            }
        });
    }

}
