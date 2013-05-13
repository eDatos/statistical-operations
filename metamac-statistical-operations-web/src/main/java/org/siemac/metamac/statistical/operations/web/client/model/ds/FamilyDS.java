package org.siemac.metamac.statistical.operations.web.client.model.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class FamilyDS extends DataSource {

    // IDENTIFIERS
    public static final String ID                      = "id";
    public static final String CODE                    = "code";
    public static final String CODE_VIEW               = "code-view";       // Not mapped in DTO
    public static final String URN                     = "fam-urn";
    public static final String TITLE                   = "title";
    public static final String ACRONYM                 = "acronym";
    // CONTENT DESCRIPTORS
    public static final String DESCRIPTION             = "description";
    // PRODUCTION DESCRIPTORS
    public static final String CREATED_DATE            = "fam-created-date";
    public static final String INTERNAL_INVENTORY_DATE = "internal-inv";
    public static final String PROC_STATUS             = "status";
    public static final String PROC_STATUS_VIEW        = "status-view";     // Not mapped in DTO
    // DIFFUSION
    public static final String INVENTORY_DATE          = "inv";

    public FamilyDS() {
        DataSourceTextField id = new DataSourceTextField(CODE);
        id.setPrimaryKey(true);
        addField(id);
    }
}
