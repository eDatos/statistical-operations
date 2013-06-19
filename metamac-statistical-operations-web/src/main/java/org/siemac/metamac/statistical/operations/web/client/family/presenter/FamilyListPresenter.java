package org.siemac.metamac.statistical.operations.web.client.family.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.List;

import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.navigation.shared.NameTokens;
import org.siemac.metamac.statistical.operations.web.client.LoggedInGatekeeper;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.events.SelectMenuButtonEvent;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.PlaceRequestUtils;
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyPaginatedListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyPaginatedListResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyResult;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;

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

public class FamilyListPresenter extends Presenter<FamilyListPresenter.FamilyListView, FamilyListPresenter.FamiliesListProxy> implements FamilyListUiHandlers {

    public final static int     FAMILY_LIST_FIRST_RESULT          = 0;
    public final static int     FAMILY_LIST_MAX_RESULTS           = 30;

    private final DispatchAsync dispatcher;
    private final PlaceManager  placeManager;

    public static final Object  TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.familyListPage)
    @UseGatekeeper(LoggedInGatekeeper.class)
    public interface FamiliesListProxy extends Proxy<FamilyListPresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle() {
        return getConstants().breadcrumbStatisticalFamilies();
    }

    public interface FamilyListView extends View, HasUiHandlers<FamilyListUiHandlers> {

        void setFamilies(List<FamilyBaseDto> familyDtos, int firstResult, int totalResults);

        HasRecordClickHandlers getSelectedFamily();
        List<Long> getSelectedFamilies();

        HasClickHandlers getSaveNewFamily();
        com.smartgwt.client.widgets.events.HasClickHandlers getDelete();

        FamilyDto getFamily();
        void onFamilySaved(FamilyDto familyDto);

        void clearSearchSection();

        boolean validate();
        void closeFamilyWindow();
    }

    @Inject
    public FamilyListPresenter(EventBus eventBus, FamilyListView familyListView, FamiliesListProxy familyListProxy, DispatchAsync dispatcher, PlaceManager placeManager) {
        super(eventBus, familyListView, familyListProxy);
        this.dispatcher = dispatcher;
        this.placeManager = placeManager;
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
                    goToFamily(record.getCode());
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
        retrieveFamilyList(FAMILY_LIST_FIRST_RESULT, FAMILY_LIST_MAX_RESULTS, null);
        SelectMenuButtonEvent.fire(this, ToolStripButtonEnum.FAMILIES);
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().statisticalFamilies());
    }

    @Override
    public void goToFamily(String familyCode) {
        if (!StringUtils.isBlank(familyCode)) {
            placeManager.revealRelativePlace(PlaceRequestUtils.buildRelativeFamilyPlaceRequest(familyCode));
        }
    }

    @Override
    public void saveFamily(FamilyDto familyToSave) {
        dispatcher.execute(new SaveFamilyAction(familyToSave), new WaitingAsyncCallback<SaveFamilyResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                getView().closeFamilyWindow();
                ShowMessageEvent.fireErrorMessage(FamilyListPresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(SaveFamilyResult result) {
                getView().closeFamilyWindow();
                getView().onFamilySaved(result.getFamilySaved());
            }
        });
    }

    @Override
    public void deleteFamilies(List<Long> familyIds) {
        dispatcher.execute(new DeleteFamilyListAction(familyIds), new WaitingAsyncCallback<DeleteFamilyListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                retrieveFamilyList(FAMILY_LIST_FIRST_RESULT, FAMILY_LIST_MAX_RESULTS, null);
                ShowMessageEvent.fireErrorMessage(FamilyListPresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(DeleteFamilyListResult result) {
                retrieveFamilyList(FAMILY_LIST_FIRST_RESULT, FAMILY_LIST_MAX_RESULTS, null);
                ShowMessageEvent.fireSuccessMessage(FamilyListPresenter.this, getMessages().familyDeleted());
            }
        });
    }

    @Override
    public void retrieveFamilyList(int firstResult, int maxResults, final String family) {
        dispatcher.execute(new GetFamilyPaginatedListAction(firstResult, maxResults, family), new AsyncCallback<GetFamilyPaginatedListResult>() {

            @Override
            public void onFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(FamilyListPresenter.this, caught);
            }

            @Override
            public void onSuccess(GetFamilyPaginatedListResult result) {
                getView().setFamilies(result.getFamilyBaseDtos(), result.getFirstResultOut(), result.getTotalResults());
                if (StringUtils.isBlank(family)) {
                    getView().clearSearchSection();
                }
            }
        });
    }
}
