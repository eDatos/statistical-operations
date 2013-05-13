package org.siemac.metamac.statistical.operations.web.client.model;

import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class FamilyRecord extends ListGridRecord {

    public FamilyRecord() {
        super();
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

    public void setUrn(String value) {
        setAttribute(FamilyDS.URN, value);
    }

    public String getTitle() {
        return getAttributeAsString(FamilyDS.TITLE);
    }

    public void setTitle(String value) {
        setAttribute(FamilyDS.TITLE, value);
    }

    public void setProcStatus(String value) {
        setAttribute(FamilyDS.PROC_STATUS, value);
    }

    public void setAcronym(String value) {
        setAttribute(FamilyDS.ACRONYM, value);
    }

    public void setCreatedDate(String value) {
        setAttribute(FamilyDS.CREATED_DATE, value);
    }

    public void setInternalInventoryDate(String value) {
        setAttribute(FamilyDS.INTERNAL_INVENTORY_DATE, value);
    }
}
