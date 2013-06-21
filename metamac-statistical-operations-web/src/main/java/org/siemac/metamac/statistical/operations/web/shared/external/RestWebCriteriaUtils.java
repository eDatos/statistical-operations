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
        return buildItemSchemeWebCriteria(new SrmItemSchemeRestCriteria(), types);
    }

    public static SrmItemSchemeRestCriteria buildItemSchemeWebCriteria(SrmItemSchemeRestCriteria itemSchemeRestCriteria, TypeExternalArtefactsEnum[] types) {
        if (areOrganisationSchemeTypes(types)) {

            OrganisationSchemeRestCriteria organisationSchemeRestCriteria = new OrganisationSchemeRestCriteria();
            organisationSchemeRestCriteria.setType(TypeExternalArtefactsEnum.ORGANISATION_SCHEME);
            organisationSchemeRestCriteria.setOrganisationSchemeTypes(types);
            organisationSchemeRestCriteria.setCriteria(itemSchemeRestCriteria.getCriteria());
            organisationSchemeRestCriteria.setIsExternallyPublished(itemSchemeRestCriteria.getIsExternallyPublished());
            organisationSchemeRestCriteria.setIsLastVersion(itemSchemeRestCriteria.getIsLastVersion());
            organisationSchemeRestCriteria.setUrn(itemSchemeRestCriteria.getUrn());
            organisationSchemeRestCriteria.setUrns(itemSchemeRestCriteria.getUrns());
            return organisationSchemeRestCriteria;

        } else if (isCodeListType(types[0])) {
            itemSchemeRestCriteria.setType(types[0]);
        } else {
            itemSchemeRestCriteria.setType(types[0]);
        }
        return itemSchemeRestCriteria;
    }

    public static SrmItemRestCriteria buildItemWebCriteria(SrmItemRestCriteria itemRestCriteria, TypeExternalArtefactsEnum[] types) {
        if (areOrganisationTypes(types)) {

            OrganisationRestCriteria organisationRestCriteria = new OrganisationRestCriteria();
            organisationRestCriteria.setType(TypeExternalArtefactsEnum.ORGANISATION);
            organisationRestCriteria.setOrganisationTypes(types);
            organisationRestCriteria.setCriteria(itemRestCriteria.getCriteria());
            organisationRestCriteria.setUrn(itemRestCriteria.getUrn());
            organisationRestCriteria.setUrns(itemRestCriteria.getUrns());
            organisationRestCriteria.setIsItemSchemeExternallyPublished(itemRestCriteria.getIsItemSchemeExternallyPublished());
            organisationRestCriteria.setIsItemSchemeLastVersion(itemRestCriteria.getIsItemSchemeLastVersion());
            organisationRestCriteria.setItemSchemeUrn(itemRestCriteria.getItemSchemeUrn());
            return organisationRestCriteria;

        } else {
            itemRestCriteria.setType(types[0]);
            return itemRestCriteria;
        }
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
