package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class ItemWebCriteria extends ExternalResourceWebCriteria {

    public ItemWebCriteria() {
    }

    public ItemWebCriteria(TypeExternalArtefactsEnum type) {
        super(type);
    }

    public ItemWebCriteria(TypeExternalArtefactsEnum type, String criteria) {
        super(type, criteria);
    }

    private static final long serialVersionUID = 1L;

    private String            itemSchemUrn;

    public String getItemSchemUrn() {
        return itemSchemUrn;
    }

    public void setItemSchemUrn(String itemSchemUrn) {
        this.itemSchemUrn = itemSchemUrn;
    }
}
