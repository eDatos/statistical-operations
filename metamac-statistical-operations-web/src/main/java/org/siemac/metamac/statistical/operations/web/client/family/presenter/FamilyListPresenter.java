package org.siemac.metamac.statistical.operations.web.client.family.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.web.client.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.ErrorUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyResult;
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

public class FamilyListPresenter extends Presenter<FamilyListPresenter.FamilyListView, FamilyListPresenter.FamiliesListProxy> implements FamilyListUiHandlers {

    private final DispatchAsync              dispatcher;
    private final PlaceManager               placeManager;

    private OperationsToolStripPresenterWidget operationsToolStripPresenterWidget;

    public static final Object               TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.familyListPage)
    public interface FamiliesListProxy extends Proxy<FamilyListPresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbStatisticalFamilies();
    }

    public interface FamilyListView extends View, HasUiHandlers<FamilyListUiHandlers> {

        void setFamilies(List<FamilyBaseDto> familyDtos);

        HasRecordClickHandlers getSelectedFamily();
        List<Long> getSelectedFamilies();

        HasClickHandlers getSaveNewFamily();
        com.smartgwt.client.widgets.events.HasClickHandlers getDelete();

        FamilyDto getFamily();
        void onFamilySaved(FamilyDto familyDto);

        boolean validate();
        void closeFamilyWindow();
    }

    @Inject
    public FamilyListPresenter(EventBus eventBus, FamilyListView familyListView, FamiliesListProxy familyListProxy, DispatchAsync dispatcher, PlaceManager placeManager,
            OperationsToolStripPresenterWidget operationsToolStripPresenterWidget) {
        super(eventBus, familyListView, familyListProxy);
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
                if (event.getFieldNum() != 0) {
                    FamilyRecord record = (FamilyRecord) event.getRecord();
                    goToFamily(record.getId());
                }
            }
        }));

        registerHandler(getView().getSaveNewFamily().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (getView().validate()) {
                    saveFamily(getView().getFamily());
                }
            }
        }));

        registerHandler(getView().getDelete().addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                deleteFamilies(getView().getSelectedFamilies());
            }
        }));
    }

    @Override
    protected void onReset() {
        super.onReset();
        retrieveFamilies();
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().statisticalFamilies());
        setInSlot(TYPE_SetContextAreaContentToolBar, operationsToolStripPresenterWidget);
    }

    @Override
    public void goToFamily(Long idFamily) {
        if (idFamily != null) {
            placeManager.revealRelativePlace(new PlaceRequest(NameTokens.familyPage).with(PlaceRequestParams.familyParam, idFamily.toString()));
        }
    }

    @Override
    public void saveFamily(FamilyDto familyToSave) {
        dispatcher.execute(new SaveFamilyAction(familyToSave), new AsyncCallback<SaveFamilyResult>() {

            @Override
            public void onFailure(Throwable caught) {
                getView().closeFamilyWindow();
                ShowMessageEvent.fire(FamilyListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorSave()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(SaveFamilyResult result) {
                getView().closeFamilyWindow();
                getView().onFamilySaved(result.getFamilySaved());
            }
        });
    }

    @Override
    public void deleteFamilies(List<Long> familyIds) {
        dispatcher.execute(new DeleteFamilyListAction(familyIds), new AsyncCallback<DeleteFamilyListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                retrieveFamilies();
                ShowMessageEvent.fire(FamilyListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familyErrorDelete()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(DeleteFamilyListResult result) {
                retrieveFamilies();
                ShowMessageEvent.fire(FamilyListPresenter.this, ErrorUtils.getMessageList(getMessages().familyDeleted()), MessageTypeEnum.SUCCESS);
            }
        });
    }

    private void retrieveFamilies() {
        dispatcher.execute(new GetFamilyListAction(), new AsyncCallback<GetFamilyListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fire(FamilyListPresenter.this, ErrorUtils.getErrorMessages(caught, getMessages().familiesErrorRetrievingData()), MessageTypeEnum.ERROR);
            }
            @Override
            public void onSuccess(GetFamilyListResult result) {
                getView().setFamilies(result.getFamilyBaseDtos());
            }
        });
    }

}
