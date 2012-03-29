package org.siemac.metamac.statistical.operations.web.client.widgets.presenter;

import org.siemac.metamac.statistical.operations.web.client.NameTokens;

import com.google.gwt.event.shared.EventBus;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.PresenterWidget;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;

public class OperationsToolStripPresenterWidget extends PresenterWidget<OperationsToolStripPresenterWidget.OperationsToolStripView> {

    private final PlaceManager placeManager;

    public interface OperationsToolStripView extends View {

        HasClickHandlers getGoFamilyList();
        HasClickHandlers getGoOperationList();
    }

    @Inject
    public OperationsToolStripPresenterWidget(EventBus eventBus, OperationsToolStripView operationsToolStripView, PlaceManager placeManager) {
        super(eventBus, operationsToolStripView);
        this.placeManager = placeManager;
    }

    @Override
    protected void onBind() {
        super.onBind();

        registerHandler(getView().getGoFamilyList().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                placeManager.revealPlace(new PlaceRequest(NameTokens.familyListPage));
            }
        }));

        registerHandler(getView().getGoOperationList().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                placeManager.revealPlace(new PlaceRequest(NameTokens.operationListPage));
            }
        }));
    }

}
