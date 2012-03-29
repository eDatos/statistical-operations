package org.siemac.metamac.gopestat.web.client.widgets;

import org.siemac.metamac.gopestat.web.client.GopestatWeb;
import org.siemac.metamac.gopestat.web.client.resources.GlobalResources;

import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class ListGridToolStrip extends ToolStrip {

    private ToolStripButton          newButton;
    private ToolStripButton          deleteButton;

    private DeleteConfirmationWindow deleteConfirmationWindow;

    public ListGridToolStrip(final String messageDeleteWindow) {
        super();
        setWidth100();

        deleteConfirmationWindow = new DeleteConfirmationWindow(new String());
        deleteConfirmationWindow.setVisibility(Visibility.HIDDEN);

        newButton = new ToolStripButton(GopestatWeb.getConstants().actionNew(), GlobalResources.RESOURCE.newListGrid().getURL());

        deleteButton = new ToolStripButton(GopestatWeb.getConstants().actionDelete(), GlobalResources.RESOURCE.deleteListGrid().getURL());
        deleteButton.setVisibility(Visibility.HIDDEN);
        deleteButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                deleteConfirmationWindow.setMessage(messageDeleteWindow);
                deleteConfirmationWindow.show();
            }
        });

        addButton(newButton);
        addSeparator();
        addButton(deleteButton);
    }

    public ToolStripButton getNewButton() {
        return newButton;
    }

    public ToolStripButton getDeleteButton() {
        return deleteButton;
    }

    public DeleteConfirmationWindow getDeleteConfirmationWindow() {
        return deleteConfirmationWindow;
    }

}
