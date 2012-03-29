package org.siemac.metamac.gopestat.web.client.widgets;

import org.siemac.metamac.gopestat.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.gopestat.web.client.GopestatWeb;
import org.siemac.metamac.gopestat.web.client.resources.GlobalResources;
import org.siemac.metamac.web.common.client.widgets.form.InternationalMainFormLayout;

import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class PublishMainFormLayout extends InternationalMainFormLayout {

    private ToolStripButton publishInternally;
    private ToolStripButton publishExternally;

    private ProcStatusEnum  status;

    public PublishMainFormLayout() {

        publishInternally = new ToolStripButton(GopestatWeb.getConstants().publishInternally());
        publishInternally.setVisibility(Visibility.HIDDEN);
        publishInternally.setWidth(150);
        publishInternally.setShowRollOver(true);
        publishInternally.setShowDisabled(true);
        publishInternally.setShowDown(true);
        publishInternally.setTitleStyle("publishButton");
        publishInternally.setIcon(GlobalResources.RESOURCE.publishInternally().getURL());

        publishExternally = new ToolStripButton(GopestatWeb.getConstants().publishExternally());
        publishExternally.setVisibility(Visibility.HIDDEN);
        publishExternally.setWidth(150);
        publishExternally.setShowRollOver(true);
        publishExternally.setShowDisabled(true);
        publishExternally.setShowDown(true);
        publishExternally.setTitleStyle("publishButton");
        publishExternally.setIcon(GlobalResources.RESOURCE.publishExternally().getURL());

        getToolStrip().addButton(publishInternally);
        getToolStrip().addButton(publishExternally);

    }

    public void updatePublishSection(ProcStatusEnum status) {
        this.status = status;
        updateVisibility();
    }

    private void updateVisibility() {
        if (ProcStatusEnum.DRAFT.equals(status)) {
            publishInternally.show();
            publishExternally.hide();
        } else if (ProcStatusEnum.PUBLISH_INTERNALLY.equals(status)) {
            publishInternally.hide();
            publishExternally.show();
        } else if (ProcStatusEnum.PUBLISH_EXTERNALLY.equals(status)) {
            publishInternally.hide();
            publishExternally.hide();
        }
    }

    public HasClickHandlers getPublishInternally() {
        return publishInternally;
    }

    public HasClickHandlers getPublishExternally() {
        return publishExternally;
    }

    public void setViewMode() {
        super.setViewMode();
        updateVisibility();
    }

    public void setEditionMode() {
        super.setEditionMode();
        publishInternally.hide();
        publishExternally.hide();
    }

}
