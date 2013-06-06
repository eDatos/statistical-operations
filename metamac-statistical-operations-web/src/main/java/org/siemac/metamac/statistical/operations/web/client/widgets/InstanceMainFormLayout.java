package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;

public class InstanceMainFormLayout extends BasePublicationMainFormLayout {

    private String operationCode;

    public InstanceMainFormLayout() {
        super();
    }

    public InstanceMainFormLayout(boolean canEdit) {
        super(canEdit);
    }

    public void setOperationCode(String operationCode) {
        this.operationCode = operationCode;
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

    @Override
    public void setViewMode() {
        super.setViewMode();
        updateVisibility();
    }

    private void showPublishInternallyButton() {
        if (ClientSecurityUtils.canPublishInstanceInternally(operationCode)) {
            publishInternally.show();
        }
    }

    private void showPublishExternallyButton() {
        if (ClientSecurityUtils.canPublishInstanceExternally(operationCode)) {
            publishExternally.show();
        }
    }
}
