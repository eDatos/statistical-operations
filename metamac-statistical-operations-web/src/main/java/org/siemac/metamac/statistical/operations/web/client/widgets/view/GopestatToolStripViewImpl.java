package org.siemac.metamac.statistical.operations.web.client.widgets.view;

import org.siemac.metamac.statistical.operations.web.client.GopestatWeb;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.GopestatToolStripPresenterWidget;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class GopestatToolStripViewImpl implements GopestatToolStripPresenterWidget.GopestatToolStripView {

    private ToolStrip       toolStrip;

    private ToolStripButton familiesButton;
    private ToolStripButton operationsButton;

    @Inject
    public GopestatToolStripViewImpl() {
        super();
        toolStrip = new ToolStrip();
        toolStrip.setWidth100();
        toolStrip.setAlign(Alignment.LEFT);

        familiesButton = new ToolStripButton(GopestatWeb.getConstants().statisticalFamilies());
        familiesButton.setAutoFit(true);

        operationsButton = new ToolStripButton(GopestatWeb.getConstants().statisticalOperations());
        operationsButton.setAutoFit(true);

        toolStrip.addButton(familiesButton);
        toolStrip.addButton(operationsButton);
    }

    @Override
    public void addToSlot(Object slot, Widget content) {

    }

    @Override
    public Widget asWidget() {
        return toolStrip;
    }

    @Override
    public void removeFromSlot(Object slot, Widget content) {

    }

    @Override
    public void setInSlot(Object slot, Widget content) {

    }

    @Override
    public HasClickHandlers getGoFamilyList() {
        return familiesButton;
    }

    @Override
    public HasClickHandlers getGoOperationList() {
        return operationsButton;
    }

}
