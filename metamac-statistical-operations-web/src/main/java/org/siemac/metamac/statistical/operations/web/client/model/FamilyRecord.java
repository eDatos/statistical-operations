package org.siemac.metamac.statistical.operations.web.client.model;

import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class FamilyRecord extends ListGridRecord {

    public FamilyRecord() {
        super();
    }

    public FamilyRecord(Long id, String code, String title, String description, String status) {
        super();
        setId(id);
        setCode(code);
        setTitle(title);
        setDescription(description);
        setStatus(status);
    }

    public Long getId() {
        return getAttributeAsLong(FamilyDS.ID);
    }

    public void setId(Long value) {
        setAttribute(FamilyDS.ID, value);
    }

    public String getCode() {
        return getAttributeAsString(FamilyDS.CODE);
    }

    public void setCode(String value) {
        setAttribute(FamilyDS.CODE, value);
    }

    public String getTitle() {
        return getAttributeAsString(FamilyDS.TITLE);
    }

    public void setTitle(String value) {
        setAttribute(FamilyDS.TITLE, value);
    }

    public String getDescription() {
        return getAttributeAsString(FamilyDS.DESCRIPTION);
    }

    public void setDescription(String value) {
        setAttribute(FamilyDS.DESCRIPTION, value);
    }

    public String getStatus() {
        return getAttributeAsString(FamilyDS.PROC_STATUS);
    }

    public void setStatus(String value) {
        setAttribute(FamilyDS.PROC_STATUS, value);
    }

}
