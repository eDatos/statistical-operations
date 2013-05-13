package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.web.common.client.utils.ListGridUtils;
import org.siemac.metamac.web.common.client.widgets.CustomListGridField;

import com.smartgwt.client.types.ListGridFieldType;

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

    public static CustomListGridField[] getOperationFields() {

        CustomListGridField code = new CustomListGridField(OperationDS.OP_CODE, getCoreMessages().operation_code());

        CustomListGridField urn = new CustomListGridField(OperationDS.OP_URN, getCoreMessages().operation_urn());
        urn.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField title = new CustomListGridField(OperationDS.OP_TITLE, getCoreMessages().operation_title());

        CustomListGridField acronym = new CustomListGridField(OperationDS.OP_ACRONYM, getCoreMessages().operation_acronym());
        acronym.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField subjectArea = new CustomListGridField(OperationDS.OP_SUBJECT_AREA, getCoreMessages().operation_subject_area());
        subjectArea.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField surveyType = new CustomListGridField(OperationDS.OP_SURVEY_TYPE, getCoreMessages().operation_survey_type());
        surveyType.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField officialityType = new CustomListGridField(OperationDS.OP_OFFICIALITY_TYPE, getCoreMessages().operation_officiality_type());
        officialityType.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField indicatorsSystem = new CustomListGridField(OperationDS.OP_INDICATOR_SYSTEM, getCoreMessages().operation_indicator_system());
        indicatorsSystem.setType(ListGridFieldType.IMAGE);
        indicatorsSystem.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField createdDate = new CustomListGridField(OperationDS.OP_CREATED_DATE, getConstants().operationCreatedDate());
        createdDate.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField internalInventoryDate = new CustomListGridField(OperationDS.OP_INTERNAL_INVENTORY_DATE, getCoreMessages().operation_internal_inventory_date());
        internalInventoryDate.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField currentlyActive = new CustomListGridField(OperationDS.OP_CURRENTLY_ACTIVE, getCoreMessages().operation_currently_active());
        currentlyActive.setShowIfCondition(ListGridUtils.getFalseListGridFieldIfFunction());

        CustomListGridField procStatus = new CustomListGridField(OperationDS.OP_PROC_STATUS, getCoreMessages().operation_proc_status());

        CustomListGridField status = new CustomListGridField(OperationDS.OP_STATUS, getCoreMessages().operation_status());

        return new CustomListGridField[]{code, urn, title, acronym, subjectArea, surveyType, officialityType, indicatorsSystem, createdDate, internalInventoryDate, currentlyActive, procStatus, status};
    }
}
