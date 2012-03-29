package org.siemac.metamac.gopestat.web.client.widgets.presenter;

import org.siemac.metamac.gopestat.web.client.NameTokens;

import com.google.gwt.event.shared.EventBus;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.PresenterWidget;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;

public class GopestatToolStripPresenterWidget extends PresenterWidget<GopestatToolStripPresenterWidget.GopestatToolStripView> {

    private final PlaceManager placeManager;

    public interface GopestatToolStripView extends View {

        HasClickHandlers getGoFamilyList();
        HasClickHandlers getGoOperationList();
    }

    @Inject
    public GopestatToolStripPresenterWidget(EventBus eventBus, GopestatToolStripView gopestatToolStripView, PlaceManager placeManager) {
        super(eventBus, gopestatToolStripView);
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
