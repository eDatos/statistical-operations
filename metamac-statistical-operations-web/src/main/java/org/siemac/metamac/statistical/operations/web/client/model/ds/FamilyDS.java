package org.siemac.metamac.statistical.operations.web.client.model.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

public class FamilyDS extends DataSource {

    // IDENTIFIERS
    public static final String IDENTIFIER              = "id";
    public static final String TITLE                   = "titleItem";
    public static final String ACRONYM                 = "titleItem-alter";
    // CONTENT DESCRIPTORS
    public static final String DESCRIPTION             = "description";
    // PRODUCTION DESCRIPTORS
    public static final String INTERNAL_INVENTORY_DATE = "internal-inv";
    public static final String STATUS                  = "status";
    // DIFFUSION
    public static final String INVENTORY_DATE          = "inv";

    public FamilyDS() {

        DataSourceTextField id = new DataSourceTextField(IDENTIFIER);
        id.setPrimaryKey(true);
        addField(id);

    }

}
