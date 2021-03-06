package org.siemac.metamac.statistical.operations.web.client.enums;

import com.smartgwt.client.types.ValueEnum;

public enum ToolStripButtonEnum implements ValueEnum {

    FAMILIES("families_button"), OPERATIONS("operations_button");

    private String value;

    ToolStripButtonEnum(String value) {
        this.value = value;
    }

    @Override
    public String getValue() {
        return this.value;
    }

}
