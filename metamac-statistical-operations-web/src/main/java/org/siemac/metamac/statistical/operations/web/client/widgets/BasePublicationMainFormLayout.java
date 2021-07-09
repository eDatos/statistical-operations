package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.web.common.client.widgets.form.InternationalMainFormLayout;

import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class BasePublicationMainFormLayout extends InternationalMainFormLayout {

    protected ToolStripButton publishInternally;
    protected ToolStripButton publishExternally;
    protected ToolStripButton lifeCycleReSendStreamMessage;

    protected ProcStatusEnum  status;

    public BasePublicationMainFormLayout() {
        super();
        common();
    }

    public BasePublicationMainFormLayout(boolean canEdit) {
        super(canEdit);
        common();
    }

    private void common() {
        publishInternally = new ToolStripButton(OperationsWeb.getConstants().publishInternally());
        publishInternally.setVisibility(Visibility.HIDDEN);
        publishInternally.setWidth(150);
        publishInternally.setShowRollOver(true);
        publishInternally.setShowDisabled(true);
        publishInternally.setShowDown(true);
        publishInternally.setTitleStyle("publishButton");
        publishInternally.setIcon(GlobalResources.RESOURCE.publishInternally().getURL());

        publishExternally = new ToolStripButton(OperationsWeb.getConstants().publishExternally());
        publishExternally.setVisibility(Visibility.HIDDEN);
        publishExternally.setWidth(150);
        publishExternally.setShowRollOver(true);
        publishExternally.setShowDisabled(true);
        publishExternally.setShowDown(true);
        publishExternally.setTitleStyle("publishButton");
        publishExternally.setIcon(GlobalResources.RESOURCE.publishExternally().getURL());

        lifeCycleReSendStreamMessage = new ToolStripButton(OperationsWeb.getConstants().lifeCycleReSendStreamMessage());
        lifeCycleReSendStreamMessage.setVisibility(Visibility.HIDDEN);
        lifeCycleReSendStreamMessage.setWidth(150);
        lifeCycleReSendStreamMessage.setShowRollOver(true);
        lifeCycleReSendStreamMessage.setShowDisabled(true);
        lifeCycleReSendStreamMessage.setShowDown(true);
        lifeCycleReSendStreamMessage.setTitleStyle("republishButton");
        lifeCycleReSendStreamMessage.setIcon(GlobalResources.RESOURCE.reload().getURL());

        getToolStrip().addButton(lifeCycleReSendStreamMessage);
        getToolStrip().addButton(publishInternally);
        getToolStrip().addButton(publishExternally);

    }

    public HasClickHandlers getPublishInternally() {
        return publishInternally;
    }

    public HasClickHandlers getPublishExternally() {
        return publishExternally;
    }

    public HasClickHandlers getLifeCycleReSendStreamMessage() {
        return lifeCycleReSendStreamMessage;
    }

    public void setEditionMode() {
        super.setEditionMode();
        publishInternally.hide();
        publishExternally.hide();
    }

}
