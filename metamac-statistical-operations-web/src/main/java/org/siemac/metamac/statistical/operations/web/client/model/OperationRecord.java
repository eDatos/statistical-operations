package org.siemac.metamac.statistical.operations.web.client.model;

import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.web.common.client.resources.GlobalResources;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class OperationRecord extends ListGridRecord {

    public OperationRecord() {
        super();
    }

    public OperationRecord(Long id, String code, String title, String acronym, String status, Boolean indicatorsSystem) {
        super();
        setId(id);
        setCode(code);
        setTitle(title);
        setAcronym(acronym);
        setStatus(status);
        setIndicatorsSystem(indicatorsSystem);
    }

    public Long getId() {
        return getAttributeAsLong(OperationDS.OP_ID);
    }

    public void setId(Long value) {
        setAttribute(OperationDS.OP_ID, value);
    }

    public String getCode() {
        return getAttributeAsString(OperationDS.OP_CODE);
    }

    public void setCode(String value) {
        setAttribute(OperationDS.OP_CODE, value);
    }

    public String getTitle() {
        return getAttributeAsString(OperationDS.OP_TITLE);
    }

    public void setTitle(String value) {
        setAttribute(OperationDS.OP_TITLE, value);
    }

    public String getAcronym() {
        return getAttributeAsString(OperationDS.OP_ACRONYM);
    }

    public void setAcronym(String value) {
        setAttribute(OperationDS.OP_ACRONYM, value);
    }

    public String getStatus() {
        return getAttributeAsString(OperationDS.OP_STATUS);
    }

    public void setStatus(String value) {
        setAttribute(OperationDS.OP_STATUS, value);
    }

    public void setIndicatorsSystem(Boolean value) {
        if (value != null && value) {
            setAttribute(OperationDS.OP_INDICATOR_SYSTEM, GlobalResources.RESOURCE.success().getURL());
        }
    }

}
