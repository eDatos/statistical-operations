package org.siemac.metamac.statistical.operations.web.client.widgets;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchCategoryItem;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.view.handlers.SrmExternalResourcesUiHandlers;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomButtonItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewOperationForm extends CustomDynamicForm {

    private static final int   FORM_ITEM_CUSTOM_WIDTH = 300;

    private RequiredTextItem   identifier;
    private RequiredTextItem   title;
    private CustomCheckboxItem releaseCalendar;
    private SearchCategoryItem subjectAreasItem;
    private CustomCheckboxItem indSystem;
    private CustomButtonItem   saveButton;

    public NewOperationForm() {
        super();

        setValidateOnChange(true);

        identifier = new RequiredTextItem("op-id", getCoreMessages().operation_code());
        identifier.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        identifier.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        title = new RequiredTextItem("op-title", getCoreMessages().operation_title());
        title.setWidth(FORM_ITEM_CUSTOM_WIDTH);

        releaseCalendar = new CustomCheckboxItem("op-release-cal", getConstants().operationReleaseCalendar());
        releaseCalendar.setWidth(FORM_ITEM_CUSTOM_WIDTH);

        subjectAreasItem = new SearchCategoryItem("op-subject", getCoreMessages().operation_subject_area());
        subjectAreasItem.setRequired(true);
        subjectAreasItem.setSaveClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                ExternalItemDto category = subjectAreasItem.getSelectedItem();
                subjectAreasItem.markSearchWindowForDestroy();
                subjectAreasItem.setExternalItem(category);
                NewOperationForm.this.validate(false);
            }
        });

        indSystem = new CustomCheckboxItem("op-ind-sys", getCoreMessages().operation_indicator_system());
        indSystem.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        indSystem.setTitleStyle("requiredFormLabel");

        saveButton = new CustomButtonItem("op-save", getConstants().actionCreateOperation());
        saveButton.setWidth(110);

        setHeight100();
        setWidth100();
        setPadding(5);
        setMargin(5);
        setErrorOrientation(FormErrorOrientation.RIGHT);
        setLayoutAlign(VerticalAlignment.BOTTOM);
        setFields(identifier, title, subjectAreasItem, releaseCalendar, indSystem, saveButton);
    }

    public OperationDto getOperation() {
        OperationDto operationDto = new OperationDto();
        operationDto.setCode(identifier.getValueAsString());
        operationDto.setTitle(InternationalStringUtils.updateInternationalString(new InternationalStringDto(), title.getValueAsString()));
        operationDto.setReleaseCalendar(releaseCalendar.getValueAsBoolean());
        operationDto.setProcStatus(ProcStatusEnum.DRAFT);
        operationDto.setSubjectArea(subjectAreasItem.getExternalItemDto());
        operationDto.setIndicatorSystem(indSystem.getValueAsBoolean() == null ? false : indSystem.getValueAsBoolean());
        return operationDto;
    }

    public HasClickHandlers getSave() {
        return saveButton;
    }

    public void setUiHandlers(SrmExternalResourcesUiHandlers uiHandlers) {
        subjectAreasItem.setUiHandlers(uiHandlers);
    }

    // ------------------------------------------------------------------------------------------------------------
    // EXTERNAL RESOURCES DATA SETTERS
    // ------------------------------------------------------------------------------------------------------------

    public void setItemSchemes(String formItemName, ExternalItemsResult result) {
        subjectAreasItem.setItemSchemes(result);
    }

    public void setItems(String formItemName, ExternalItemsResult result) {
        subjectAreasItem.setItems(result);
    }
}
