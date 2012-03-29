package org.siemac.metamac.gopestat.web.client.widgets;

import org.siemac.metamac.gopestat.web.client.GopestatWeb;
import org.siemac.metamac.gopestat.web.client.resources.GlobalResources;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

public class DeleteConfirmationWindow extends Window {

    private Label   label;

    private IButton yesButton;

    public DeleteConfirmationWindow(String message) {
        super();
        setWidth(330);
        setHeight(130);
        setTitle(GopestatWeb.getConstants().confirmDelete());
        setShowMinimizeButton(false);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();
        addCloseClickHandler(new CloseClickHandler() {

            @Override
            public void onCloseClick(CloseClickEvent event) {
                hide();
            }
        });

        label = new Label(message);
        label.setAutoHeight();
        label.setIcon(GlobalResources.RESOURCE.ask().getURL());
        label.setIconSize(32);
        label.setIconSpacing(10);

        yesButton = new IButton(GopestatWeb.getConstants().yes());
        yesButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                DeleteConfirmationWindow.this.hide();
            }
        });

        IButton noButton = new IButton(GopestatWeb.getConstants().no());
        noButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                DeleteConfirmationWindow.this.hide();
            }
        });

        HLayout buttonsLayout = new HLayout(2);
        buttonsLayout.addMember(yesButton);
        buttonsLayout.addMember(noButton);
        buttonsLayout.setAlign(Alignment.CENTER);

        VLayout layout = new VLayout();
        layout.setMembersMargin(20);
        layout.addMember(label);
        layout.addMember(buttonsLayout);
        layout.setMargin(10);

        addItem(layout);
    }

    public void setMessage(String message) {
        label.setContents(message);
    }

    public IButton getYesButton() {
        return yesButton;
    }

}
