package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class OrganisationWebCriteria extends ItemWebCriteria {

    private static final long           serialVersionUID = 1L;

    private TypeExternalArtefactsEnum[] organisationTypes;

    public OrganisationWebCriteria() {
    }

    public TypeExternalArtefactsEnum[] getOrganisationTypes() {
        return organisationTypes;
    }

    public void setOrganisationTypes(TypeExternalArtefactsEnum[] organisationTypes) {
        this.organisationTypes = organisationTypes;
    }
}
