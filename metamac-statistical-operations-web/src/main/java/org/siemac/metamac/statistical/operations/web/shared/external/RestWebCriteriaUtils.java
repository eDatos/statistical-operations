package org.siemac.metamac.statistical.operations.web.shared.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.AGENCY;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.AGENCY_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CODELIST;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_CONSUMER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_CONSUMER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;

public class RestWebCriteriaUtils {

    //
    // INSTACE: STATISTICAL UNIT
    //

    public static ConceptSchemeRestCriteria buildStatisticalUnitConceptSchemeWebCriteria() {
        ConceptSchemeRestCriteria conceptSchemeWebCriteria = new ConceptSchemeRestCriteria();
        conceptSchemeWebCriteria.setType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        conceptSchemeWebCriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.GLOSSARY, ConceptSchemeTypeEnum.OPERATION);
        return conceptSchemeWebCriteria;
    }

    public static ConceptRestCriteria buildStatisticalUnitConceptWebCriteria() {
        ConceptRestCriteria conceptWebcriteria = new ConceptRestCriteria();
        conceptWebcriteria.setType(TypeExternalArtefactsEnum.CONCEPT);
        conceptWebcriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.GLOSSARY, ConceptSchemeTypeEnum.OPERATION);
        return conceptWebcriteria;
    }

    //
    // INSTACE: STAT CONC DEF
    //

    public static ConceptSchemeRestCriteria buildStatConcDefConceptSchemeWebCriteria() {
        ConceptSchemeRestCriteria conceptSchemeWebCriteria = new ConceptSchemeRestCriteria();
        conceptSchemeWebCriteria.setType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        conceptSchemeWebCriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.OPERATION);
        return conceptSchemeWebCriteria;
    }

    public static ConceptRestCriteria buildStatConcDefConceptWebCriteria() {
        ConceptRestCriteria conceptWebcriteria = new ConceptRestCriteria();
        conceptWebcriteria.setType(TypeExternalArtefactsEnum.CONCEPT);
        conceptWebcriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.TRANSVERSAL, ConceptSchemeTypeEnum.OPERATION);
        return conceptWebcriteria;
    }

    //
    // INSTACE: MEASURES
    //

    public static ConceptSchemeRestCriteria buildMeasuresConceptSchemeWebCriteria() {
        ConceptSchemeRestCriteria conceptSchemeWebCriteria = new ConceptSchemeRestCriteria();
        conceptSchemeWebCriteria.setType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        conceptSchemeWebCriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.MEASURE);
        return conceptSchemeWebCriteria;
    }

    public static ConceptRestCriteria buildMeasuresConceptWebCriteria() {
        ConceptRestCriteria conceptWebcriteria = new ConceptRestCriteria();
        conceptWebcriteria.setType(TypeExternalArtefactsEnum.CONCEPT);
        conceptWebcriteria.setConceptSchemeTypes(ConceptSchemeTypeEnum.MEASURE);
        return conceptWebcriteria;
    }

    //
    // COMMON CRITERIA
    //

    public static SrmItemSchemeRestCriteria buildItemSchemeWebCriteria(TypeExternalArtefactsEnum... types) {
        return buildItemSchemeWebCriteria(types, null);
    }

    public static SrmItemSchemeRestCriteria buildItemSchemeWebCriteria(TypeExternalArtefactsEnum[] types, String criteria) {
        SrmItemSchemeRestCriteria externalResourceWebCriteria = new SrmItemSchemeRestCriteria();
        if (areOrganisationSchemeTypes(types)) {
            externalResourceWebCriteria = new OrganisationSchemeRestCriteria();
            externalResourceWebCriteria.setType(TypeExternalArtefactsEnum.ORGANISATION_SCHEME);
            ((OrganisationSchemeRestCriteria) externalResourceWebCriteria).setOrganisationSchemeTypes(types);
        } else if (isCodeListType(types[0])) {
            externalResourceWebCriteria = new SrmItemSchemeRestCriteria(types[0], criteria);
        } else {
            externalResourceWebCriteria.setType(types[0]);
        }
        externalResourceWebCriteria.setCriteria(criteria);
        return externalResourceWebCriteria;
    }

    public static SrmItemRestCriteria buildItemWebCriteria(TypeExternalArtefactsEnum[] types, String criteria, String itemSchemeUrn) {
        SrmItemRestCriteria itemWebCriteria = new SrmItemRestCriteria();
        if (areOrganisationTypes(types)) {
            itemWebCriteria = new OrganisationRestCriteria();
            itemWebCriteria.setType(TypeExternalArtefactsEnum.ORGANISATION);
            ((OrganisationRestCriteria) itemWebCriteria).setOrganisationTypes(types);
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

    private static boolean isCodeListType(TypeExternalArtefactsEnum type) {
        return CODELIST.equals(type);
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
