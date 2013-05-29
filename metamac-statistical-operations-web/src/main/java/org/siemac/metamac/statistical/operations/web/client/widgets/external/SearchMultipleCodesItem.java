package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CODE;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CODELIST;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;

public class SearchMultipleCodesItem extends SearchMultipleItemsItem {

    public SearchMultipleCodesItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(CODELIST), RestWebCriteriaUtils.buildItemWebCriteria(new TypeExternalArtefactsEnum[]{CODE}, null, null), getConstants()
                .searchCodes(), getConstants().filterCodelist(), getConstants().selectedCodelist(), getConstants().selectionCodes(), action);
    }
}
