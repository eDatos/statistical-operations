package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.web.common.client.utils.ListGridUtils;
import org.siemac.metamac.web.common.client.widgets.CustomListGridField;

public class ResourceListFieldUtils {

    public static CustomListGridField[] getFamilyFields() {

        CustomListGridField code = new CustomListGridField(FamilyDS.CODE, getCoreMessages().family_code());

        CustomListGridField urn = new CustomListGridField(FamilyDS.URN, getCoreMessages().family_urn());
        urn.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField title = new CustomListGridField(FamilyDS.TITLE, getCoreMessages().family_title());

        CustomListGridField acronym = new CustomListGridField(FamilyDS.ACRONYM, getCoreMessages().family_acronym());
        acronym.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField createdDate = new CustomListGridField(FamilyDS.CREATED_DATE, getConstants().familyCreatedDate());
        createdDate.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField internalInventoryDate = new CustomListGridField(FamilyDS.INTERNAL_INVENTORY_DATE, getCoreMessages().family_internal_inventory_date());
        internalInventoryDate.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField procStatus = new CustomListGridField(FamilyDS.PROC_STATUS, getCoreMessages().family_proc_status());

        return new CustomListGridField[]{code, urn, title, acronym, createdDate, internalInventoryDate, procStatus};
    }
}
