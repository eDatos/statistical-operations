package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.MainPageUiHandlers;
import org.siemac.metamac.web.common.client.widgets.RadioToolStripButton;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

public class OperationsMenu extends ToolStrip {

    private RadioToolStripButton familiesButton;
    private RadioToolStripButton operationsButton;

    private MainPageUiHandlers   uiHandlers;

    public OperationsMenu() {
        setWidth100();
        setAlign(Alignment.LEFT);

        familiesButton = new RadioToolStripButton(OperationsWeb.getConstants().statisticalFamilies());
        familiesButton.setID(ToolStripButtonEnum.FAMILIES.getValue());
        familiesButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                getUiHandlers().goToFamilies();
            }
        });

        operationsButton = new RadioToolStripButton(OperationsWeb.getConstants().statisticalOperations());
        operationsButton.setID(ToolStripButtonEnum.OPERATIONS.getValue());
        operationsButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                getUiHandlers().goToOperations();
            }
        });

        addButton(familiesButton);
        addButton(operationsButton);
    }

    public MainPageUiHandlers getUiHandlers() {
        return uiHandlers;
    }

    public void setUiHandlers(MainPageUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }
}
