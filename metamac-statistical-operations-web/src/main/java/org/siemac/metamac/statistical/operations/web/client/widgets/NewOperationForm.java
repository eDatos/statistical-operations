package org.siemac.metamac.statistical.operations.web.client.widgets;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewOperationForm extends CustomDynamicForm {

    private RequiredTextItem        identifier;
    private RequiredTextItem        title;
    private CustomCheckboxItem      releaseCalendar;
    private ExternalSelectItem      subjectAreasItem;
    private CustomCheckboxItem      indSystem;
    private ButtonItem              saveButton;

    private List<ExternalItemBtDto> subjectAreas;

    public NewOperationForm() {
        super();
        
        setValidateOnChange(true);

        identifier = new RequiredTextItem("op-id", getConstants().operationIdentifier());
        identifier.setWidth(200);
        identifier.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        title = new RequiredTextItem("op-title", getConstants().operationTitle());
        title.setWidth(200);

        releaseCalendar = new CustomCheckboxItem("op-release-cal", getConstants().operationReleaseCalendar());

        subjectAreasItem = new ExternalSelectItem("op-subject", getConstants().operationSubjectArea());
        subjectAreasItem.setRequired(true);

        indSystem = new CustomCheckboxItem("op-ind-sys", getConstants().operationIndicatorSystem());
        indSystem.setTitleStyle("requiredFormLabel");

        saveButton = new ButtonItem("op-save", getConstants().actionCreateOperation());
        saveButton.setAlign(Alignment.RIGHT);
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

    public void setSubjectAreasSchemes(List<ExternalItemBtDto> schemes) {
        subjectAreasItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    public void setSubjetcAreas(List<ExternalItemBtDto> subjects) {
        this.subjectAreas = subjects;
        subjectAreasItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(subjects));
    }

}
