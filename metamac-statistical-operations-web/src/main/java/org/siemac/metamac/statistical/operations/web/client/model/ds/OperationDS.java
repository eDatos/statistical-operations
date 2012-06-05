package org.siemac.metamac.statistical.operations.web.client.model.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class OperationDS extends DataSource {

    // IDENTIFIERS
    public static final String OP_ID                        = "op-id";
    public static final String OP_CODE                      = "op-code";
    public static final String OP_CODE_VIEW                 = "op-code-view";       // Not mapped in DTO
    public static final String OP_TITLE                     = "op-title";
    public static final String OP_ACRONYM                   = "op-acron";
    // CONTENT CLASSIFIERS
    public static final String OP_SUBJECT                   = "op-subjectItem";
    public static final String OP_SUBJECT_SECONDARY         = "op-subjetc-secon";
    // CONTENT DESCRIPTORS
    public static final String OP_DESCRIPTION               = "op-desc";
    public static final String OP_OBJECTIVE                 = "op-obj";
    // CLASS DESCRIPTORS
    public static final String OP_SURVEY_TYPE               = "op-sur";
    public static final String OP_OFFICIALITY_TYPE          = "op-off-type";
    public static final String OP_INDICATOR_SYSTEM          = "op-ind-system";
    // PRODUCTION DESCRIPTORS
    public static final String OP_PRODUCER                  = "op-producer";
    public static final String OP_REG_RESPONSIBLE           = "op-reg-resp";
    public static final String OP_REG_CONTRIBUTOR           = "op-reg-con";
    public static final String OP_INTERNAL_INVENTORY_DATE   = "op-int-inv-date";
    public static final String OP_CURRENTLY_ACTIVE          = "op-currently-active";
    public static final String OP_STATUS                    = "op-status";
    public static final String OP_PROC_STATUS               = "op-proc-status";
    public static final String OP_PROC_STATUS_VIEW          = "op-proc-status-view"; // Not mapped in DTO
    // DIFUSSION DESCRIPTORS
    public static final String OP_PUBLISHER                 = "op-publisherItem";
    public static final String OP_RE_POL_US_AC              = "op-pol-us";
    public static final String OP_RELEASE_CALENDAR          = "op-calendar";
    public static final String OP_RELEASE_CALENDAR_ACCESS   = "op-calendar-access";
    public static final String OP_UPDATE_FREQ               = "op-up-freq";
    public static final String OP_CURRENT_INSTANCE          = "op-current-inst";
    public static final String OP_CURRENT_INTERNAL_INSTANCE = "op-current-in-inst";
    public static final String OP_INVENTORY_DATE            = "op-inv-date";
    public static final String OP_REV_POLICY                = "op-rev-pol";
    public static final String OP_REV_PRACTICE              = "op-rev-pract";
    public static final String OP_COMMON_METADATA           = "op-com-met";
    // ANNOTATIONS
    public static final String OP_COMMENTS                  = "op-com";
    public static final String OP_NOTES                     = "op-not";

    public OperationDS() {

        DataSourceTextField id = new DataSourceTextField(OP_ID);
        id.setPrimaryKey(true);
        addField(id);

    }

}
