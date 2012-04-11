package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewFamilyForm extends DynamicForm {

    private RequiredTextItem identifier;
    private RequiredTextItem title;
    private ButtonItem       saveButton;

    public NewFamilyForm() {
        super();

        setValidateOnChange(true);
        
        identifier = new RequiredTextItem("fam-id", OperationsWeb.getConstants().familyIdentifier());
        identifier.setWidth(200);
        identifier.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        title = new RequiredTextItem("fam-title", OperationsWeb.getConstants().familyTitle());
        title.setWidth(200);

        saveButton = new ButtonItem("fam-save", OperationsWeb.getConstants().actionCreateFamily());
        saveButton.setAlign(Alignment.RIGHT);
        saveButton.setWidth(110);

        setHeight100();
        setWidth100();
        setPadding(5);
        setMargin(5);
        setErrorOrientation(FormErrorOrientation.RIGHT);
        setLayoutAlign(VerticalAlignment.BOTTOM);
        setFields(identifier, title, saveButton);

    }

    public FamilyDto getFamily() {
        FamilyDto familyDto = new FamilyDto();
        familyDto.setCode(identifier.getValueAsString());
        familyDto.setTitle(InternationalStringUtils.updateInternationalString(new InternationalStringDto(), title.getValueAsString()));
        familyDto.setProcStatus(ProcStatusEnum.DRAFT);
        return familyDto;
    }

    public HasClickHandlers getSave() {
        return saveButton;
    }

}
