package org.siemac.metamac.statistical.operations.web.server.rest.utils;

import org.apache.commons.lang.ArrayUtils;
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
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeTypeEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

public class RestQueryUtils {

    //
    // CATEGORY SCHEME
    //

    public static String buildCategorySchemeQuery(ExternalResourceWebCriteria externalResourceWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = externalResourceWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CategorySchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategorySchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategorySchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // CATEGORY
    //

    public static String buildCategoryQuery(SrmItemRestCriteria itemWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(itemWebCriteria.getItemSchemUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(CategoryCriteriaPropertyRestriction.CATEGORY_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemWebCriteria.getItemSchemUrn())
                    .append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // CODELIST
    //

    public static String buildCodelistQuery(ExternalResourceWebCriteria externalResourceWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = externalResourceWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CodelistCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodelistCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodelistCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // CODE
    //

    public static String buildCodeQuery(SrmItemRestCriteria itemWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(CodeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(CodeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(itemWebCriteria.getItemSchemUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(CodeCriteriaPropertyRestriction.CODELIST_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemWebCriteria.getItemSchemUrn()).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // CONCEPT SCHEME
    //

    public static String buildConceptSchemeQuery(ConceptSchemeWebCriteria conceptSchemeWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = conceptSchemeWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (!ArrayUtils.isEmpty(conceptSchemeWebCriteria.getConceptSchemeTypes())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }

            ConceptSchemeTypeEnum[] conceptSchemeTypes = conceptSchemeWebCriteria.getConceptSchemeTypes();

            queryBuilder.append("(");

            for (int i = 0; i < conceptSchemeTypes.length; i++) {

                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }

                if (ConceptSchemeTypeEnum.OPERATION.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptSchemeWebCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is OPERATION and the statistical operation is the specified

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptSchemeWebCriteria.getStatisticalOperationUrn()).append("\"");
                    queryBuilder.append(")");

                } else if (ConceptSchemeTypeEnum.MEASURE.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptSchemeWebCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is MEASURE and the statistical operation is the specified

                    queryBuilder.append("(");

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptSchemeWebCriteria.getStatisticalOperationUrn()).append("\"");
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

    public static String buildConceptQuery(ConceptWebCriteria conceptWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = conceptWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(conceptWebCriteria.getItemSchemUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(conceptWebCriteria.getItemSchemUrn())
                    .append("\"");
            queryBuilder.append(")");
        }
        if (!ArrayUtils.isEmpty(conceptWebCriteria.getConceptSchemeTypes())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }

            ConceptSchemeTypeEnum[] conceptSchemeTypes = conceptWebCriteria.getConceptSchemeTypes();

            queryBuilder.append("(");

            for (int i = 0; i < conceptSchemeTypes.length; i++) {

                if (i != 0) {
                    queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
                }

                if (ConceptSchemeTypeEnum.OPERATION.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptWebCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is OPERATION and the statistical operation is the specified

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptWebCriteria.getStatisticalOperationUrn()).append("\"");
                    queryBuilder.append(")");

                } else if (ConceptSchemeTypeEnum.MEASURE.equals(conceptSchemeTypes[i]) && StringUtils.isNotBlank(conceptWebCriteria.getStatisticalOperationUrn())) {

                    // Concept scheme type is MEASURE and the statistical operation is the specified

                    queryBuilder.append("(");

                    queryBuilder.append("(");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(getConceptSchemeType(conceptSchemeTypes[i])).append("\"");
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                    queryBuilder.append(ConceptCriteriaPropertyRestriction.CONCEPT_SCHEME_STATISTICAL_OPERATION_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                            .append(conceptWebCriteria.getStatisticalOperationUrn()).append("\"");
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
        return queryBuilder.toString();
    }

    //
    // ORGANISATION SCHEME
    //

    public static String buildOrganisationSchemeQuery(ExternalResourceWebCriteria externalResourceWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = externalResourceWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (externalResourceWebCriteria instanceof OrganisationSchemeWebCriteria && !ArrayUtils.isEmpty(((OrganisationSchemeWebCriteria) externalResourceWebCriteria).getOrganisationSchemeTypes())) {

            TypeExternalArtefactsEnum[] organisationSchemeTypes = ((OrganisationSchemeWebCriteria) externalResourceWebCriteria).getOrganisationSchemeTypes();

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

    public static String buildOrganisationQuery(SrmItemRestCriteria itemWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(itemWebCriteria.getItemSchemUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.ORGANISATION_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                    .append(itemWebCriteria.getItemSchemUrn()).append("\"");
            queryBuilder.append(")");
        }
        if (itemWebCriteria instanceof OrganisationWebCriteria && !ArrayUtils.isEmpty(((OrganisationWebCriteria) itemWebCriteria).getOrganisationTypes())) {

            TypeExternalArtefactsEnum[] organisationTypes = ((OrganisationWebCriteria) itemWebCriteria).getOrganisationTypes();

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
