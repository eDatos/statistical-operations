package org.siemac.metamac.statistical.operations.web.server.rest.utils;

import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.rest.common.v1_0.domain.ComparisonOperator;
import org.siemac.metamac.rest.common.v1_0.domain.LogicalOperator;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategoryCriteriaPropertyRestriction;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemeCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;

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
            // TODO add condition
        }
        return queryBuilder.toString();
    }
}
