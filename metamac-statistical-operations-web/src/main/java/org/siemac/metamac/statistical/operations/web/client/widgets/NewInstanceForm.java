package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewInstanceForm extends CustomDynamicForm {

    private RequiredTextItem identifier;
    private RequiredTextItem title;
    private ButtonItem       saveButton;

    public NewInstanceForm() {
        super();

        setValidateOnChange(true);

        identifier = new RequiredTextItem("in-id", OperationsWeb.getConstants().instanceIdentifier());
        identifier.setWidth(200);
        identifier.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        title = new RequiredTextItem("in-title", OperationsWeb.getConstants().instanceTitle());
        title.setWidth(200);

        saveButton = new ButtonItem("op-save", OperationsWeb.getConstants().actionCreateInstance());
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

    public InstanceDto getInstance() {
        InstanceDto instanceDto = new InstanceDto();
        instanceDto.setCode(identifier.getValueAsString());
        instanceDto.setTitle(InternationalStringUtils.updateInternationalString(new InternationalStringDto(), title.getValueAsString()));
        instanceDto.setProcStatus(ProcStatusEnum.DRAFT);
        return instanceDto;
    }

    public HasClickHandlers getSave() {
        return saveButton;
    }

}
