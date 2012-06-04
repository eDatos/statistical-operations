package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;

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

    public void setViewMode() {
        super.setViewMode();
        updateVisibility();
    }

    private void showPublishInternallyButton() {
        publishInternally.show();
    }

    private void showPublishExternallyButton() {
        publishExternally.show();
    }

}
