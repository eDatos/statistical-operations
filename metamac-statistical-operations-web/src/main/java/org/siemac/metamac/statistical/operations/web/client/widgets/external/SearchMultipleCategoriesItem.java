package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CATEGORY;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CATEGORY_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;

public class SearchMultipleCategoriesItem extends SearchMultipleItemsItem {

    public SearchMultipleCategoriesItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(CATEGORY_SCHEME), RestWebCriteriaUtils.buildItemWebCriteria(new TypeExternalArtefactsEnum[]{CATEGORY}, null, null),
                getConstants().searchCategories(), getConstants().filterCategoryScheme(), getConstants().selectedCategoryScheme(), getConstants().selectionCategories(), action);
    }
}
