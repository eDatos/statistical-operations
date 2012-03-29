package org.siemac.metamac.gopestat.web.client.model;

import com.smartgwt.client.widgets.grid.ListGridRecord;

public class OperationRecord extends ListGridRecord {

    public static final String ID      = "id";
    public static final String CODE    = "code";
    public static final String TITLE   = "title";
    public static final String ACRONYM = "acron";
    public static final String STATUS  = "status";

    public OperationRecord() {
        super();
    }

    public OperationRecord(Long id, String code, String title, String acronym, String status) {
        super();
        setId(id);
        setCode(code);
        setTitle(title);
        setAcronym(acronym);
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

    public String getAcronym() {
        return getAttributeAsString(ACRONYM);
    }

    public void setAcronym(String value) {
        setAttribute(ACRONYM, value);
    }

    public String getStatus() {
        return getAttributeAsString(STATUS);
    }

    public void setStatus(String value) {
        setAttribute(STATUS, value);
    }

}
