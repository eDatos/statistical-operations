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
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationSchemeCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationSchemeType;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationType;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;

public class RestCriteriaUtils {

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

    public static String buildCategoryQuery(ItemWebCriteria itemWebCriteria) {
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
            queryBuilder.append(CategoryCriteriaPropertyRestriction.ITEM_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemWebCriteria.getItemSchemUrn())
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

    public static String buildCodeQuery(ItemWebCriteria itemWebCriteria) {
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
            queryBuilder.append(CodeCriteriaPropertyRestriction.ITEM_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemWebCriteria.getItemSchemUrn()).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // CONCEPT SCHEME
    //

    public static String buildConceptSchemeQuery(ExternalResourceWebCriteria externalResourceWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = externalResourceWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptSchemeCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // CONCEPT
    //

    public static String buildConceptQuery(ItemWebCriteria itemWebCriteria) {
        StringBuilder queryBuilder = new StringBuilder();
        String criteria = itemWebCriteria.getCriteria();
        if (StringUtils.isNotBlank(criteria)) {
            queryBuilder.append("(");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.ID).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.NAME).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(" ").append(LogicalOperator.OR.name()).append(" ");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.URN).append(" ").append(ComparisonOperator.ILIKE.name()).append(" \"").append(criteria).append("\"");
            queryBuilder.append(")");
        }
        if (StringUtils.isNotBlank(itemWebCriteria.getItemSchemUrn())) {
            if (StringUtils.isNotBlank(queryBuilder.toString())) {
                queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
            }
            queryBuilder.append("(");
            queryBuilder.append(ConceptCriteriaPropertyRestriction.ITEM_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemWebCriteria.getItemSchemUrn())
                    .append("\"");
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
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                }
                queryBuilder.append(OrganisationSchemeCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"")
                        .append(getOrganisationSchemeType(organisationSchemeTypes[i])).append("\"");
            }
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    //
    // ORGANISATION
    //

    public static String buildOrganisationQuery(ItemWebCriteria itemWebCriteria) {
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
            queryBuilder.append(OrganisationCriteriaPropertyRestriction.ITEM_SCHEME_URN).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(itemWebCriteria.getItemSchemUrn())
                    .append("\"");
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
                    queryBuilder.append(" ").append(LogicalOperator.AND.name()).append(" ");
                }
                queryBuilder.append(OrganisationCriteriaPropertyRestriction.TYPE).append(" ").append(ComparisonOperator.EQ.name()).append(" \"").append(getOrganisationType(organisationTypes[i]))
                        .append("\"");
            }
            queryBuilder.append(")");
        }
        return queryBuilder.toString();
    }

    private static OrganisationSchemeType getOrganisationSchemeType(TypeExternalArtefactsEnum typeExternalArtefactsEnum) {
        switch (typeExternalArtefactsEnum) {
            case AGENCY:
                return OrganisationSchemeType.AGENCY_SCHEME;
            case DATA_PROVIDER:
                return OrganisationSchemeType.DATA_PROVIDER_SCHEME;
            case DATA_CONSUMER:
                return OrganisationSchemeType.DATA_CONSUMER_SCHEME;
            case ORGANISATION_UNIT:
                return OrganisationSchemeType.ORGANISATION_UNIT_SCHEME;
            default:
                return null;
        }
    }

    private static OrganisationType getOrganisationType(TypeExternalArtefactsEnum typeExternalArtefactsEnum) {
        switch (typeExternalArtefactsEnum) {
            case AGENCY:
                return OrganisationType.AGENCY;
            case DATA_PROVIDER:
                return OrganisationType.DATA_PROVIDER;
            case DATA_CONSUMER:
                return OrganisationType.DATA_CONSUMER;
            case ORGANISATION_UNIT:
                return OrganisationType.ORGANISATION_UNIT;
            default:
                return null;
        }
    }
}
