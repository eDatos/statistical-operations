package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;

public class OrganisationSchemeWebCriteria extends ExternalResourceWebCriteria {

    private static final long           serialVersionUID = 1L;

    private TypeExternalArtefactsEnum[] organisationSchemeTypes;

    public OrganisationSchemeWebCriteria() {
    }

    public TypeExternalArtefactsEnum[] getOrganisationSchemeTypes() {
        return organisationSchemeTypes;
    }

    public void setOrganisationSchemeTypes(TypeExternalArtefactsEnum[] organisationSchemeTypes) {
        this.organisationSchemeTypes = organisationSchemeTypes;
    }
}
