package org.siemac.metamac.statistical.operations.web.server.rest.utils;

import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.ComparisonOperator;
import org.siemac.metamac.rest.common.v1_0.domain.LogicalOperator;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategoryCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemeCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CodeCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CodelistCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ConceptCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ConceptSchemeCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ConceptSchemeType;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationSchemeCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationSchemeType;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationType;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeTypeEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;

public class RestQueryUtils {

    //
    // CATEGORY SCHEME
    //

    public static String buildCategorySchemeQuery(SrmItemSchemeRestCriteria itemSchemeRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemSchemeRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CategorySchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategorySchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategorySchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(CategorySchemeCriteriaPropertyRestriction.LATEST, itemSchemeRestCriteria.getIsLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    //
    // CATEGORY
    //

    public static String buildCategoryQuery(SrmItemRestCriteria itemRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }

        // Filter by category scheme
        if (StringUtils.isNotBlank(itemRestCriteria.getItemSchemeUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.CATEGORY_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemRestCriteria.getItemSchemeUrn())
                    .append("\"");
            queryBuilder.append(")");
        }

        // Find categories with one of the specified URNs
        String urnsQuery = buildUrnsQuery(CategoryCriteriaPropertyRestriction.URN, itemRestCriteria.getUrns());
        addConditionToQueryBuilderIfAny(queryBuilder, urnsQuery, LogicalOperator.AND);

        // Find categories that are externally published
        String externallyPublishedQuery = buildBooleanQuery(CategoryCriteriaPropertyRestriction.CATEGORY_SCHEME_EXTERNALLY_PUBLISHED, itemRestCriteria.getIsItemSchemeExternallyPublished());
        addConditionToQueryBuilderIfAny(queryBuilder, externallyPublishedQuery, LogicalOperator.AND);

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(CategoryCriteriaPropertyRestriction.CATEGORY_SCHEME_LATEST, itemRestCriteria.getIsItemSchemeLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    //
    // CODELIST
    //

    public static String buildCodelistQuery(SrmItemSchemeRestCriteria itemSchemeRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemSchemeRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CodelistCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodelistCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodelistCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }

        // Find codelists with one of the specified URNs
        String urnsQuery = buildUrnsQuery(CodelistCriteriaPropertyRestriction.URN, itemSchemeRestCriteria.getUrns());
        addConditionToQueryBuilderIfAny(queryBuilder, urnsQuery, LogicalOperator.AND);

        // Find codelists that are externally published
        if (BooleanUtils.isTrue(itemSchemeRestCriteria.getIsExternallyPublished())) {
            String query = buildSrmProcStatusQuery(CodelistCriteriaPropertyRestriction.PROC_STATUS, ProcStatus.EXTERNALLY_PUBLISHED);
            addConditionToQueryBuilderIfAny(queryBuilder, query, LogicalOperator.AND);
        }

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(CodelistCriteriaPropertyRestriction.LATEST, itemSchemeRestCriteria.getIsLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    //
    // CODE
    //

    public static String buildCodeQuery(SrmItemRestCriteria itemRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CodeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(itemRestCriteria.getItemSchemeUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(CodeCriteriaPropertyRestriction.CODELIST_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemRestCriteria.getItemSchemeUrn()).append("\"");
            queryBuilder.append(")");
        }

        // Find codes with one of the specified URNs
        String urnsQuery = buildUrnsQuery(CodeCriteriaPropertyRestriction.URN, itemRestCriteria.getUrns());
        addConditionToQueryBuilderIfAny(queryBuilder, urnsQuery, LogicalOperator.AND);

        // Find codes that are externally published
        String externallyPublishedQuery = buildBooleanQuery(CodeCriteriaPropertyRestriction.CODELIST_EXTERNALLY_PUBLISHED, itemRestCriteria.getIsItemSchemeExternallyPublished());
        addConditionToQueryBuilderIfAny(queryBuilder, externallyPublishedQuery, LogicalOperator.AND);

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(CodeCriteriaPropertyRestriction.CODELIST_LATEST, itemRestCriteria.getIsItemSchemeLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    //
    // CONCEPT SCHEME
    //

    public static String buildConceptSchemeQuery(ConceptSchemeRestCriteria conceptSchemeRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = conceptSchemeRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (!ArrayUtils.isEmpty(conceptSchemeRestCriteria.getConceptSchemeTypes())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }

            ConceptSchemeTypeEnum[] conceptSchemeTypes = conceptSchemeRestCriteria.getConceptSchemeTypes();

            queryBuilder.append("(");

            for (int i = 0; i < conceptSchemeTypes.length; i++) {

                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }

                if (ConceptSchemeTypeEnum.OPERATION.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptSchemeRestCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is OPERATION and the statistical operation is the specified

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptSchemeRestCriteria.getStatisticalOperationUrn()).append("\"");
                    queryBuilder.append(")");

                } else if (ConceptSchemeTypeEnum.MEASURE.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptSchemeRestCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is MEASURE and the statistical operation is the specified

                    queryBuilder.append("(");

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptSchemeRestCriteria.getStatisticalOperationUrn()).append("\"");
                    queryBuilder.append(")");

                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");

                    // Concept scheme type is MEASURE

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.IS_NULL.name());
                    queryBuilder.append(")");

                    queryBuilder.append(")");
                } else {
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                }
            }

            queryBuilder.append(")");
        }

        // Find concept Schemes with one of the specified URNs
        String urnsQuery = buildUrnsQuery(ConceptSchemeCriteriaPropertyRestriction.URN, conceptSchemeRestCriteria.getUrns());
        addConditionToQueryBuilderIfAny(queryBuilder, urnsQuery, LogicalOperator.AND);

        // Find concept Schemes that are externally published
        if (BooleanUtils.isTrue(conceptSchemeRestCriteria.getIsExternallyPublished())) {
            String externallyPublishedQuery = buildSrmProcStatusQuery(ConceptSchemeCriteriaPropertyRestriction.PROC_STATUS, ProcStatus.EXTERNALLY_PUBLISHED);
            addConditionToQueryBuilderIfAny(queryBuilder, externallyPublishedQuery, LogicalOperator.AND);
        }

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(ConceptSchemeCriteriaPropertyRestriction.LATEST, conceptSchemeRestCriteria.getIsLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    private static String getConceptSchemeType(ConceptSchemeTypeEnum type) {
        switch (type) {
            case GLOSSARY:
                return ConceptSchemeType.GLOSSARY.name();
            case MEASURE:
                return ConceptSchemeType.MEASURE.name();
            case OPERATION:
                return ConceptSchemeType.OPERATION.name();
            case ROLE:
                return ConceptSchemeType.ROLE.name();
            case TRANSVERSAL:
                return ConceptSchemeType.TRANSVERSAL.name();
            default:
                return null;
        }
    }

    //
    // CONCEPT
    //

    public static String buildConceptQuery(ConceptRestCriteria conceptRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = conceptRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }

        // Filter by concept scheme

        if (StringUtils.isNotBlank(conceptRestCriteria.getItemSchemeUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(conceptRestCriteria.getItemSchemeUrn())
                    .append("\"");
            queryBuilder.append(")");
        }

        // Filter by concept scheme types

        if (!ArrayUtils.isEmpty(conceptRestCriteria.getConceptSchemeTypes())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }

            ConceptSchemeTypeEnum[] conceptSchemeTypes = conceptRestCriteria.getConceptSchemeTypes();

            queryBuilder.append("(");

            for (int i = 0; i < conceptSchemeTypes.length; i++) {

                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }

                if (ConceptSchemeTypeEnum.OPERATION.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptRestCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is OPERATION and the statistical operation is the specified

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptRestCriteria.getStatisticalOperationUrn()).append("\"");
                    queryBuilder.append(")");

                } else if (ConceptSchemeTypeEnum.MEASURE.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptRestCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is MEASURE and the statistical operation is the specified

                    queryBuilder.append("(");

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptRestCriteria.getStatisticalOperationUrn()).append("\"");
                    queryBuilder.append(")");

                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");

                    // Concept scheme type is MEASURE

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.IS_NULL.name());
                    queryBuilder.append(")");

                    queryBuilder.append(")");

                } else {
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                }
            }
            queryBuilder.append(")");
        }

        // Find concepts with one of the specified URNs
        String urnsQuery = buildUrnsQuery(ConceptCriteriaPropertyRestriction.URN, conceptRestCriteria.getUrns());
        addConditionToQueryBuilderIfAny(queryBuilder, urnsQuery, LogicalOperator.AND);

        // Find concepts that are externally published
        String externallyPublishedQuery = buildBooleanQuery(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_EXTERNALLY_PUBLISHED, conceptRestCriteria.getIsItemSchemeExternallyPublished());
        addConditionToQueryBuilderIfAny(queryBuilder, externallyPublishedQuery, LogicalOperator.AND);

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_LATEST, conceptRestCriteria.getIsItemSchemeLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    //
    // ORGANISATION SCHEME
    //

    public static String buildOrganisationSchemeQuery(SrmItemSchemeRestCriteria itemSchemeRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemSchemeRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (itemSchemeRestCriteria instanceof OrganisationSchemeRestCriteria && !ArrayUtils.isEmpty(((OrganisationSchemeRestCriteria) itemSchemeRestCriteria).getOrganisationSchemeTypes())) {

            TypeExternalArtefactsEnum[] organisationSchemeTypes = ((OrganisationSchemeRestCriteria) itemSchemeRestCriteria).getOrganisationSchemeTypes();

            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            for (int i = 0; i < organisationSchemeTypes.length; i++) {
                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }
                queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                        .append(getOrganisationSchemeType(organisationSchemeTypes[i])).append("\"");
            }
            queryBuilder.append(")");
        }

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(OrganisationSchemeCriteriaPropertyRestriction.LATEST, itemSchemeRestCriteria.getIsLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    private static String getOrganisationSchemeType(TypeExternalArtefactsEnum typeExternalArtefactsEnum) {
        switch (typeExternalArtefactsEnum) {
            case AGENCY_SCHEME:
                return OrganisationSchemeType.AGENCY_SCHEME.name();
            case DATA_PROVIDER_SCHEME:
                return OrganisationSchemeType.DATA_PROVIDER_SCHEME.name();
            case DATA_CONSUMER_SCHEME:
                return OrganisationSchemeType.DATA_CONSUMER_SCHEME.name();
            case ORGANISATION_UNIT_SCHEME:
                return OrganisationSchemeType.ORGANISATION_UNIT_SCHEME.name();
            default:
                return null;
        }
    }

    //
    // ORGANISATION
    //

    public static String buildOrganisationQuery(SrmItemRestCriteria itemRestCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemRestCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(itemRestCriteria.getItemSchemeUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.ORGANISATION_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                    .append(itemRestCriteria.getItemSchemeUrn()).append("\"");
            queryBuilder.append(")");
        }
        if (itemRestCriteria instanceof OrganisationRestCriteria && !ArrayUtils.isEmpty(((OrganisationRestCriteria) itemRestCriteria).getOrganisationTypes())) {

            TypeExternalArtefactsEnum[] organisationTypes = ((OrganisationRestCriteria) itemRestCriteria).getOrganisationTypes();

            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            for (int i = 0; i < organisationTypes.length; i++) {
                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }
                queryBuilder.append(OrganisationCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(getOrganisationType(organisationTypes[i]))
                        .append("\"");
            }
            queryBuilder.append(")");
        }

        // Find organizations with one of the specified URNs
        String urnsQuery = buildUrnsQuery(OrganisationCriteriaPropertyRestriction.URN, itemRestCriteria.getUrns());
        addConditionToQueryBuilderIfAny(queryBuilder, urnsQuery, LogicalOperator.AND);

        // Find concepts that are externally published
        String externallyPublishedQuery = buildBooleanQuery(OrganisationCriteriaPropertyRestriction.ORGANISATION_SCHEME_EXTERNALLY_PUBLISHED, itemRestCriteria.getIsItemSchemeExternallyPublished());
        addConditionToQueryBuilderIfAny(queryBuilder, externallyPublishedQuery, LogicalOperator.AND);

        // Only last versions
        String lastVersionQuery = buildIsLastVersionQuery(OrganisationCriteriaPropertyRestriction.ORGANISATION_SCHEME_LATEST, itemRestCriteria.getIsItemSchemeLastVersion());
        addConditionToQueryBuilderIfAny(queryBuilder, lastVersionQuery, LogicalOperator.AND);

        return queryBuilder.toString();
    }

    private static void addConditionToQueryBuilderIfAny(StringBuilder queryBuilder, String condition, LogicalOperator operator) {
        if (StringUtils.isNotBlank(condition)) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(operator.name()).append(" ");
            }
            queryBuilder.append(condition);
        }
    }

    @SuppressWarnings("rawtypes")
    private static String buildUrnsQuery(Enum urnPropertyEnum, List<String> urns) {
        StringBuilder queryBuilder = new StringBuilder();
        if (urns != null && !urns.isEmpty()) {
            queryBuilder.append("(");
            for (int i = 0; i < urns.size(); i++) {
                String urn = urns.get(i);
                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }
                queryBuilder.append(urnPropertyEnum).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(urn).append("\"");
            }
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    @SuppressWarnings("rawtypes")
    private static String buildSrmProcStatusQuery(Enum procStatusPropertyEnum, ProcStatus procStatus) {
        StringBuilder queryBuilder = new StringBuilder();
        if (procStatus != null) {
            queryBuilder.append("(");
            queryBuilder.append(procStatusPropertyEnum).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(procStatus).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    @SuppressWarnings("rawtypes")
    private static String buildBooleanQuery(Enum propertyEnum, Boolean booleanValue) {
        StringBuilder queryBuilder = new StringBuilder();
        if (booleanValue != null) {
            queryBuilder.append("(");
            queryBuilder.append(propertyEnum).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(booleanValue).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    @SuppressWarnings("rawtypes")
    private static String buildIsLastVersionQuery(Enum propertyEnum, Boolean booleanValue) {
        StringBuilder queryBuilder = new StringBuilder();
        if (BooleanUtils.isTrue(booleanValue)) {
            queryBuilder.append("(");
            queryBuilder.append(propertyEnum).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(booleanValue).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    private static String getOrganisationType(TypeExternalArtefactsEnum typeExternalArtefactsEnum) {
        switch (typeExternalArtefactsEnum) {
            case AGENCY:
                return OrganisationType.AGENCY.name();
            case DATA_PROVIDER:
                return OrganisationType.DATA_PROVIDER.name();
            case DATA_CONSUMER:
                return OrganisationType.DATA_CONSUMER.name();
            case ORGANISATION_UNIT:
                return OrganisationType.ORGANISATION_UNIT.name();
            default:
                return null;
        }
    }
}
