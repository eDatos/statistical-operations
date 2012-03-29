package org.siemac.metamac.statistical.operations.web.client.model;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class FamilyRecord extends ListGridRecord {

    public static final String ID          = "id";
    public static final String CODE        = "code";
    public static final String TITLE       = "title";
    public static final String DESCRIPTION = "description";
    public static final String STATUS      = "status";

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
        return getAttributeAsLong(ID);
    }

    public void setId(Long value) {
        setAttribute(ID, value);
    }

    public String getCode() {
        return getAttributeAsString(CODE);
    }

    public void setCode(String value) {
        setAttribute(CODE, value);
    }

    public String getTitle() {
        return getAttributeAsString(TITLE);
    }

    public void setTitle(String value) {
        setAttribute(TITLE, value);
    }

    public String getDescription() {
        return getAttributeAsString(DESCRIPTION);
    }

    public void setDescription(String value) {
        setAttribute(DESCRIPTION, value);
    }

    public String getStatus() {
        return getAttributeAsString(STATUS);
    }

    public void setStatus(String value) {
        setAttribute(STATUS, value);
    }

}
