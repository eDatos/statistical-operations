package org.siemac.metamac.statistical.operations.web.client.widgets;

import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomButtonItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;

import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewInstanceForm extends CustomDynamicForm {

    private RequiredTextItem identifier;
    private RequiredTextItem title;
    private CustomButtonItem saveButton;

    public NewInstanceForm() {
        super();

        setValidateOnChange(true);

        identifier = new RequiredTextItem("in-id", OperationsWeb.getCoreMessages().instance_code());
        identifier.setWidth(200);
        identifier.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        title = new RequiredTextItem("in-title", OperationsWeb.getCoreMessages().instance_title());
        title.setWidth(200);

        saveButton = new CustomButtonItem("op-save", OperationsWeb.getConstants().actionCreateInstance());
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
