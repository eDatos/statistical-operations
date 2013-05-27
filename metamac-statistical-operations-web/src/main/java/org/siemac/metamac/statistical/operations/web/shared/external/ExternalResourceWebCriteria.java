package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.shared.criteria.MetamacWebCriteria;

public class ExternalResourceWebCriteria extends MetamacWebCriteria {

    private static final long         serialVersionUID = 1L;

    private TypeExternalArtefactsEnum type;

    public ExternalResourceWebCriteria() {
    }

    public ExternalResourceWebCriteria(TypeExternalArtefactsEnum type) {
        super();
        this.type = type;
    }

    public ExternalResourceWebCriteria(TypeExternalArtefactsEnum type, String criteria) {
        super(criteria);
        this.type = type;
    }

    public TypeExternalArtefactsEnum getType() {
        return type;
    }

    public void setType(TypeExternalArtefactsEnum type) {
        this.type = type;
    }
}
