package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.AGENCY;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.AGENCY_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_CONSUMER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_CONSUMER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;

public class RestCriteriaClientUtils {

    public static ExternalResourceWebCriteria buildItemSchemeWebCriteria(TypeExternalArtefactsEnum[] types, String criteria) {
        ExternalResourceWebCriteria externalResourceWebCriteria = new ExternalResourceWebCriteria();
        if (areOrganisationSchemeTypes(types)) {
            externalResourceWebCriteria = new OrganisationSchemeWebCriteria();
            externalResourceWebCriteria.setType(TypeExternalArtefactsEnum.ORGANISATION_SCHEME);
            ((OrganisationSchemeWebCriteria) externalResourceWebCriteria).setOrganisationSchemeTypes(types);
        } else {
            externalResourceWebCriteria.setType(types[0]);
        }
        externalResourceWebCriteria.setCriteria(criteria);
        return externalResourceWebCriteria;
    }

    public static ExternalResourceWebCriteria buildItemWebCriteria(TypeExternalArtefactsEnum[] types, String criteria, String itemSchemeUrn) {
        ItemWebCriteria itemWebCriteria = new ItemWebCriteria();
        if (areOrganisationTypes(types)) {
            itemWebCriteria = new OrganisationWebCriteria();
            itemWebCriteria.setType(TypeExternalArtefactsEnum.ORGANISATION);
            ((OrganisationWebCriteria) itemWebCriteria).setOrganisationTypes(types);
        } else {
            itemWebCriteria.setType(types[0]);
        }
        itemWebCriteria.setItemSchemUrn(itemSchemeUrn);
        itemWebCriteria.setCriteria(criteria);
        return itemWebCriteria;
    }

    private static boolean areOrganisationSchemeTypes(TypeExternalArtefactsEnum[] types) {
        for (TypeExternalArtefactsEnum type : types) {
            if (!ORGANISATION_UNIT_SCHEME.equals(type) && !AGENCY_SCHEME.equals(type) && !DATA_PROVIDER_SCHEME.equals(type) && !DATA_CONSUMER_SCHEME.equals(type)) {
                return false;
            }
        }
        return true;
    }

    private static boolean areOrganisationTypes(TypeExternalArtefactsEnum[] types) {
        for (TypeExternalArtefactsEnum type : types) {
            if (!ORGANISATION_UNIT.equals(type) && !AGENCY.equals(type) && !DATA_PROVIDER.equals(type) && !DATA_CONSUMER.equals(type)) {
                return false;
            }
        }
        return true;
    }
}
