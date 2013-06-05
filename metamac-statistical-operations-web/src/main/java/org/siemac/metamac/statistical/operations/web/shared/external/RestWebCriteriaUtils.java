package org.siemac.metamac.statistical.operations.web.shared.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.AGENCY;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.AGENCY_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_CONSUMER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_CONSUMER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

public class RestWebCriteriaUtils {

    //
    // INSTACE: STATISTICAL UNIT
    //

    public static ConceptSchemeWebCriteria buildStatisticalUnitConceptSchemeWebCriteria() {
        ConceptSchemeWebCriteria conceptSchemeWebCriteria = new ConceptSchemeWebCriteria();
        conceptSchemeWebCriteria.setType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        conceptSchemeWebCriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.GLOSSARY, ConceptSchemeTypeEnum.OPERATION);
        return conceptSchemeWebCriteria;
    }

    public static ConceptWebCriteria buildStatisticalUnitConceptWebCriteria() {
        ConceptWebCriteria conceptWebcriteria = new ConceptWebCriteria();
        conceptWebcriteria.setType(TypeExternalArtefactsEnum.CONCEPT);
        conceptWebcriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.GLOSSARY, ConceptSchemeTypeEnum.OPERATION);
        return conceptWebcriteria;
    }

    //
    // INSTACE: STAT CONC DEF
    //

    public static ConceptSchemeWebCriteria buildStatConcDefConceptSchemeWebCriteria() {
        ConceptSchemeWebCriteria conceptSchemeWebCriteria = new ConceptSchemeWebCriteria();
        conceptSchemeWebCriteria.setType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        conceptSchemeWebCriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.OPERATION);
        return conceptSchemeWebCriteria;
    }

    public static ConceptWebCriteria buildStatConcDefConceptWebCriteria() {
        ConceptWebCriteria conceptWebcriteria = new ConceptWebCriteria();
        conceptWebcriteria.setType(TypeExternalArtefactsEnum.CONCEPT);
        conceptWebcriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.OPERATION);
        return conceptWebcriteria;
    }

    //
    // INSTACE: MEASURES
    //

    public static ConceptSchemeWebCriteria buildMeasuresConceptSchemeWebCriteria() {
        ConceptSchemeWebCriteria conceptSchemeWebCriteria = new ConceptSchemeWebCriteria();
        conceptSchemeWebCriteria.setType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        conceptSchemeWebCriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.MEASURE);
        return conceptSchemeWebCriteria;
    }

    public static ConceptWebCriteria buildMeasuresConceptWebCriteria() {
        ConceptWebCriteria conceptWebcriteria = new ConceptWebCriteria();
        conceptWebcriteria.setType(TypeExternalArtefactsEnum.CONCEPT);
        conceptWebcriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.MEASURE);
        return conceptWebcriteria;
    }

    //
    // COMMON CRITERIA
    //

    public static ExternalResourceWebCriteria buildItemSchemeWebCriteria(TypeExternalArtefactsEnum... types) {
        return buildItemSchemeWebCriteria(types, null);
    }

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

    public static SrmItemRestCriteria buildItemWebCriteria(TypeExternalArtefactsEnum[] types, String criteria, String itemSchemeUrn) {
        SrmItemRestCriteria itemWebCriteria = new SrmItemRestCriteria();
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
