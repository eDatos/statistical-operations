package org.siemac.metamac.statistical.operations.web.client.widgets.view;

import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.widgets.presenter.OperationsToolStripPresenterWidget;
import org.siemac.metamac.web.common.client.widgets.RadioToolStripButton;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

public class OperationsToolStripViewImpl implements OperationsToolStripPresenterWidget.OperationsToolStripView {

    private ToolStrip            toolStrip;

    private RadioToolStripButton familiesButton;
    private RadioToolStripButton operationsButton;

    @Inject
    public OperationsToolStripViewImpl() {
        super();
        toolStrip = new ToolStrip();
        toolStrip.setWidth100();
        toolStrip.setAlign(Alignment.LEFT);

        familiesButton = new RadioToolStripButton(OperationsWeb.getConstants().statisticalFamilies());
        familiesButton.setID(ToolStripButtonEnum.FAMILIES.getValue());

        operationsButton = new RadioToolStripButton(OperationsWeb.getConstants().statisticalOperations());
        operationsButton.setID(ToolStripButtonEnum.OPERATIONS.getValue());

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
