package org.siemac.metamac.statistical.operations.web.client.view;

import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.MainPageUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.widgets.OperationsMenu;
import org.siemac.metamac.web.common.client.enums.MessageTypeEnum;
import org.siemac.metamac.web.common.client.widgets.BreadCrumbsPanel;
import org.siemac.metamac.web.common.client.widgets.FooterLayout;
import org.siemac.metamac.web.common.client.widgets.MasterHead;
import org.siemac.metamac.web.common.client.widgets.MessagePanel;
import org.siemac.metamac.web.common.client.widgets.MetamacNavBar;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class MainPageViewImpl extends ViewWithUiHandlers<MainPageUiHandlers> implements MainPagePresenter.MainPageView {

    private static final int       NORTH_HEIGHT   = 85;
    private static final String    DEFAULT_MARGIN = "0px";

    private MainPageUiHandlers     uiHandlers;

    private final MasterHead       masterHead;
    private final OperationsMenu   operationsMenu;
    private final BreadCrumbsPanel breadCrumbsPanel;

    private final MessagePanel     messagePanel;

    private VLayout                panel;
    private VLayout                northLayout;
    private HLayout                southLayout;
    private FooterLayout           footerLayout;

    @Inject
    public MainPageViewImpl(MasterHead masterHead, OperationsMenu operationsMenu, BreadCrumbsPanel breadCrumbsPanel, MessagePanel messagePanel) {
        this.masterHead = masterHead;
        this.operationsMenu = operationsMenu;
        this.breadCrumbsPanel = breadCrumbsPanel;
        this.messagePanel = messagePanel;
        // get rid of scroll bars, and clear out the window's built-in margin,
        // because we want to take advantage of the entire client area
        Window.enableScrolling(false);
        Window.setMargin(DEFAULT_MARGIN);

        // Initialize the main layout container
        panel = new VLayout();
        panel.setWidth100();
        panel.setHeight100();
        panel.setAlign(Alignment.CENTER);
        panel.setCanDrag(false);

        // Initialize the North layout container
        northLayout = new VLayout();
        northLayout.setHeight(NORTH_HEIGHT);

        // Nested layout container
        VLayout vLayout = new VLayout();
        vLayout.addMember(this.masterHead);
        vLayout.addMember(this.operationsMenu);

        VLayout breadCrumbLayout = new VLayout();
        breadCrumbLayout.addMember(this.breadCrumbsPanel);
        breadCrumbLayout.setMargin(10);
        vLayout.addMember(breadCrumbLayout);

        // Nested layout container to the North layout container
        northLayout.addMember(this.masterHead);
        northLayout.addMember(vLayout);
        northLayout.setHeight(65);

        // Initialize the South layout container
        southLayout = new HLayout();
        southLayout.setHeight100();

        footerLayout = new FooterLayout(this.messagePanel, OperationsWeb.getProjectVersion());

        // Set user name
        masterHead.getUserNameLabel().setContents(getUserName());

        // Add handlers to logout button
        masterHead.getLogoutLink().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                uiHandlers.closeSession();
            }
        });

        // Help section
        masterHead.getHelpLink().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                uiHandlers.openHelpUrl();
            }
        });

        MetamacNavBar navBar = new MetamacNavBar();
        northLayout.setZIndex(0);
        southLayout.setZIndex(0);
        footerLayout.setZIndex(0);
        // Add the North and South layout containers to the main layout
        // container
        panel.addMember(navBar);
        panel.addMember(northLayout);
        panel.addMember(southLayout);
        panel.addMember(footerLayout);
    }

    @Override
    public Widget asWidget() {
        return panel;
    }

    /****************************************************
     * Code for nested presenters.
     ***************************************************/

    /*
     * GWTP will call setInSlot when a child presenter asks to be added under this view
     */
    @Override
    public void setInSlot(Object slot, Widget content) {
        if (slot == MainPagePresenter.TYPE_SetContextAreaContent) {
            if (content != null) {
                southLayout.setMembers((VLayout) content);
            }
        } else {
            // To support inheritance in your views it is good practice to call super.setInSlot when you can't handle the call.
            // Who knows, maybe the parent class knows what to do with this slot.
            super.setInSlot(slot, content);
        }
    }

    @Override
    public void removeFromSlot(Object slot, Widget content) {
        super.removeFromSlot(slot, content);
    }

    /****************************************************
     * End code for nested presenters.
     ***************************************************/

    @Override
    public MasterHead getMasterHead() {
        return masterHead;
    }

    @Override
    public void clearBreadcrumbs(int size, PlaceManager placeManager) {
        breadCrumbsPanel.clearBreadCrumbs(size, placeManager);
    }

    @Override
    public void setBreadcrumbs(int index, String title) {
        breadCrumbsPanel.setBreadCrumbs(index, title);
    }

    @Override
    public void showMessage(Throwable throwable, String message, MessageTypeEnum type) {
        messagePanel.showMessage(throwable, message, type);
    }

    @Override
    public void hideMessages() {
        messagePanel.hide();
    }

    @Override
    public void setUiHandlers(MainPageUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
        this.operationsMenu.setUiHandlers(uiHandlers);
    }

    private String getUserName() {
        MetamacPrincipal metamacPrincipal = OperationsWeb.getCurrentUser();
        if (metamacPrincipal != null) {
            return metamacPrincipal.getUserId();
        }
        return new String();
    }

    @Override
    public void selectStructuralResourceMenuButton(ToolStripButtonEnum type) {
        Canvas[] canvas = operationsMenu.getMembers();
        for (int i = 0; i < canvas.length; i++) {
            if (canvas[i] instanceof ToolStripButton) {
                if (type != null && type.getValue().equals(((ToolStripButton) canvas[i]).getID())) {
                    ((ToolStripButton) canvas[i]).select();
                } else {
                    ((ToolStripButton) canvas[i]).deselect();
                }
            }
        }
    }
}
