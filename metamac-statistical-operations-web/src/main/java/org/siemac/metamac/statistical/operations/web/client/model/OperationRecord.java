package org.siemac.metamac.statistical.operations.web.client.model;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.statistical.operations.core.dto.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.web.common.client.resources.GlobalResources;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class OperationRecord extends ListGridRecord {

    public OperationRecord() {
        super();
    }

    public void setId(Long value) {
        setAttribute(OperationDS.OP_ID, value);
    }

    public Long getId() {
        return getAttributeAsLong(OperationDS.OP_ID);
    }

    public String getCode() {
        return getAttributeAsString(OperationDS.OP_CODE);
    }

    public void setCode(String value) {
        setAttribute(OperationDS.OP_CODE, value);
    }

    public void setUrn(String value) {
        setAttribute(OperationDS.OP_URN, value);
    }

    public void setTitle(String value) {
        setAttribute(OperationDS.OP_TITLE, value);
    }

    public void setAcronym(String value) {
        setAttribute(OperationDS.OP_ACRONYM, value);
    }

    public void setProcStatus(String value) {
        setAttribute(OperationDS.OP_PROC_STATUS, value);
    }

    public void setStatus(String value) {
        setAttribute(OperationDS.OP_STATUS, value);
    }

    public void setIndicatorsSystem(Boolean value) {
        if (value != null && value) {
            setAttribute(OperationDS.OP_INDICATOR_SYSTEM, GlobalResources.RESOURCE.success().getURL());
        }
    }

    public void setSubjectArea(ExternalItemDto value) {
        if (value != null) {
            setAttribute(OperationDS.OP_SUBJECT_AREA, CommonWebUtils.getElementName(value.getCode(), value.getTitle()));
        }
    }

    public void setSurveyType(SurveyTypeDto value) {
        if (value != null) {
            setAttribute(OperationDS.OP_STATISTICAL_OPERATION_TYPE, CommonWebUtils.getElementName(value.getIdentifier(), value.getDescription()));
        }
    }

    public void setOfficialityType(OfficialityTypeDto value) {
        if (value != null) {
            setAttribute(OperationDS.OP_OFFICIALITY_TYPE, CommonWebUtils.getElementName(value.getIdentifier(), value.getDescription()));
        }
    }

    public void setCreatedDate(String value) {
        setAttribute(OperationDS.OP_CREATED_DATE, value);
    }

    public void setInternalInventoryDate(String value) {
        setAttribute(OperationDS.OP_INTERNAL_INVENTORY_DATE, value);
    }

    public void setCurrentlyActive(String value) {
        setAttribute(OperationDS.OP_CURRENTLY_ACTIVE, value);
    }
}
