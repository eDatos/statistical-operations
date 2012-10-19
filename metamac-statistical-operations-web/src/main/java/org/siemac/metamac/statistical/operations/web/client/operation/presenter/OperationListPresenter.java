package org.siemac.metamac.statistical.operations.web.client.operation.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.LoggedInGatekeeper;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.PlaceRequestUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListResult;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesResult;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeResult;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationPaginatedListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationPaginatedListResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationResult;
import org.siemac.metamac.web.common.client.enums.MessageTypeEnum;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;

import com.google.gwt.event.shared.EventBus;
import com.google.inject.Inject;
import com.gwtplatform.dispatch.shared.DispatchAsync;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.TitleFunction;
import com.gwtplatform.mvp.client.annotations.UseGatekeeper;
import com.gwtplatform.mvp.client.proxy.Place;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.Proxy;
import com.gwtplatform.mvp.client.proxy.RevealContentEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class OperationListPresenter extends Presenter<OperationListPresenter.OperationListView, OperationListPresenter.OperationsListProxy> implements OperationListUiHandlers {

    public final static int                    OPERATION_LIST_FIRST_RESULT       = 0;
    public final static int                    OPERATION_LIST_MAX_RESULTS        = 30;

    private final DispatchAsync                dispatcher;
    private final PlaceManager                 placeManager;

    private OperationsToolStripPresenterWidget operationsToolStripPresenterWidget;

    public static final Object                 TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.operationListPage)
    @UseGatekeeper(LoggedInGatekeeper.class)
    public interface OperationsListProxy extends Proxy<OperationListPresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbStatisticalOperations();
    }

    public interface OperationListView extends View, HasUiHandlers<OperationListUiHandlers> {

        void setOperations(List<OperationBaseDto> operationBaseDtos, int firstResult, int totalResults);
        HasRecordClickHandlers getSelectedOperation();
        HasClickHandlers getSaveNewOperation();
        OperationDto getOperation();
        boolean validate();
        void closeOperationWindow();
        com.smartgwt.client.widgets.events.HasClickHandlers getDelete();
        List<Long> getSelectedOperations();
        void onOperationSaved(OperationDto operationDto);

        void clearSearchSection();

        void setCategorySchemes(List<ExternalItemDto> schemes);
        void setSubjects(List<ExternalItemDto> subjects);
    }

    @Inject
    public OperationListPresenter(EventBus eventBus, OperationListView operationListView, OperationsListProxy operationsListProxy, DispatchAsync dispatcher, PlaceManager placeManager,
            OperationsToolStripPresenterWidget operationsToolStripPresenterWidget) {
        super(eventBus, operationListView, operationsListProxy);
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
                if (event.getFieldNum() != 0) {
                    OperationRecord record = (OperationRecord) event.getRecord();
                    goToOperation(record.getCode());
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
        retrieveOperationList(OPERATION_LIST_FIRST_RESULT, OPERATION_LIST_MAX_RESULTS, null);
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().statisticalOperations());
        setInSlot(TYPE_SetContextAreaContentToolBar, operationsToolStripPresenterWidget);
    }

    @Override
    public void goToOperation(String operationCode) {
        if (!StringUtils.isBlank(operationCode)) {
            placeManager.revealRelativePlace(PlaceRequestUtils.buildOperationPlaceRequest(operationCode));
        }
    }

    @Override
    public void saveOperation(OperationDto operationToSave) {
        dispatcher.execute(new SaveOperationAction(operationToSave), new WaitingAsyncCallback<SaveOperationResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                getView().closeOperationWindow();
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(SaveOperationResult result) {
                getView().closeOperationWindow();
                getView().onOperationSaved(result.getOperationSaved());
            }
        });
    }

    @Override
    public void retrieveOperationList(int firstResult, int maxResults, final String operation) {
        dispatcher.execute(new GetOperationPaginatedListAction(firstResult, maxResults, operation), new WaitingAsyncCallback<GetOperationPaginatedListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetOperationPaginatedListResult result) {
                getView().setOperations(result.getOperationBaseDtos(), result.getFirstResultOut(), result.getTotalResults());
                if (StringUtils.isBlank(operation)) {
                    getView().clearSearchSection();
                }
            }
        });
    }

    @Override
    public void deleteOperations(List<Long> operationIds) {
        dispatcher.execute(new DeleteOperationListAction(operationIds), new WaitingAsyncCallback<DeleteOperationListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                retrieveOperationList(OPERATION_LIST_FIRST_RESULT, OPERATION_LIST_MAX_RESULTS, null);
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorDelete()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(DeleteOperationListResult result) {
                retrieveOperationList(OPERATION_LIST_FIRST_RESULT, OPERATION_LIST_MAX_RESULTS, null);
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getMessageList(getMessages().operationDeleted()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    @Override
    public void populateSubjects(String uri) {
        dispatcher.execute(new GetCategoriesFromSchemeAction(uri), new WaitingAsyncCallback<GetCategoriesFromSchemeResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().operationErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(GetCategoriesFromSchemeResult result) {
                getView().setSubjects(result.getCategories());
            }
        });
    }

    @Override
    public void retrieveCategorySchemes() {
        dispatcher.execute(new FindAllCategorySchemesAction(), new WaitingAsyncCallback<FindAllCategorySchemesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fire(OperationListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().listsErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onWaitSuccess(FindAllCategorySchemesResult result) {
                getView().setCategorySchemes(result.getCategorySchemes());
            }
        });
    }

}
