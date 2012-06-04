package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;

public class FamilyMainFormLayout extends BasePublicationMainFormLayout {

    public FamilyMainFormLayout() {
        super();
    }

    public FamilyMainFormLayout(boolean canEdit) {
        super();
    }

    public void updatePublishSection(ProcStatusEnum status) {
        this.status = status;
        updateVisibility();
    }

    private void updateVisibility() {
        if (ProcStatusEnum.DRAFT.equals(status)) {
            showPublishInternallyButton();
            publishExternally.hide();
        } else if (ProcStatusEnum.PUBLISH_INTERNALLY.equals(status)) {
            publishInternally.hide();
            showPublishExternallyButton();
        } else if (ProcStatusEnum.PUBLISH_EXTERNALLY.equals(status)) {
            publishInternally.hide();
            publishExternally.hide();
        }
    }

    public void setViewMode() {
        super.setViewMode();
        updateVisibility();
    }

    private void showPublishInternallyButton() {
        if (ClientSecurityUtils.canPublishFamilyInternally()) {
            publishInternally.show();
        }
    }

    private void showPublishExternallyButton() {
        if (ClientSecurityUtils.canPublishFamilyExternally()) {
            publishExternally.show();
        }
    }

}
