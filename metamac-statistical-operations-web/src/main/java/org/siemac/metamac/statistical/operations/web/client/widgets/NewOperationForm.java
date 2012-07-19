package org.siemac.metamac.statistical.operations.web.client.widgets;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomButtonItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;

import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewOperationForm extends CustomDynamicForm {

    private static final int      FORM_ITEM_CUSTOM_WIDTH = 300;

    private RequiredTextItem      identifier;
    private RequiredTextItem      title;
    private CustomCheckboxItem    releaseCalendar;
    private ExternalSelectItem    subjectAreasItem;
    private CustomCheckboxItem    indSystem;
    private CustomButtonItem      saveButton;

    private List<ExternalItemDto> subjectAreas;

    public NewOperationForm() {
        super();

        setValidateOnChange(true);

        identifier = new RequiredTextItem("op-id", getConstants().operationIdentifier());
        identifier.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        identifier.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        title = new RequiredTextItem("op-title", getConstants().operationTitle());
        title.setWidth(FORM_ITEM_CUSTOM_WIDTH);

        releaseCalendar = new CustomCheckboxItem("op-release-cal", getConstants().operationReleaseCalendar());
        releaseCalendar.setWidth(FORM_ITEM_CUSTOM_WIDTH);

        subjectAreasItem = new ExternalSelectItem("op-subject", getConstants().operationSubjectArea(), FORM_ITEM_CUSTOM_WIDTH);
        subjectAreasItem.setWidth(FORM_ITEM_CUSTOM_WIDTH);
        subjectAreasItem.setRequired(true);

        indSystem = new CustomCheckboxItem("op-ind-sys", getConstants().operationIndicatorSystem());
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
        setFields(identifier, title, releaseCalendar, subjectAreasItem, indSystem, saveButton);
    }

    public OperationDto getOperation() {
        OperationDto operationDto = new OperationDto();
        operationDto.setCode(identifier.getValueAsString());
        operationDto.setTitle(InternationalStringUtils.updateInternationalString(new InternationalStringDto(), title.getValueAsString()));
        operationDto.setReleaseCalendar(releaseCalendar.getValueAsBoolean());
        operationDto.setProcStatus(ProcStatusEnum.DRAFT);
        operationDto.setSubjectArea(subjectAreasItem.getSelectedExternalItem(subjectAreas));
        operationDto.setIndicatorSystem(indSystem.getValueAsBoolean() == null ? false : indSystem.getValueAsBoolean());
        return operationDto;
    }

    public HasClickHandlers getSave() {
        return saveButton;
    }

    @Override
    public boolean validate() {
        return super.validate() && subjectAreasItem.validateItem();
    }

    public ExternalSelectItem getSubjectAreasItem() {
        return subjectAreasItem;
    }

    public void setSubjectAreasSchemes(List<ExternalItemDto> schemes) {
        subjectAreasItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    public void setSubjetcAreas(List<ExternalItemDto> subjects) {
        this.subjectAreas = subjects;
        subjectAreasItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(subjects));
    }

}
