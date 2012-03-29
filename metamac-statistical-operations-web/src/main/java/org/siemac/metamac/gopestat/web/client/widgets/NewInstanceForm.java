package org.siemac.metamac.gopestat.web.client.widgets;

import org.siemac.metamac.core.common.dto.serviceapi.InternationalStringDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.gopestat.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.gopestat.web.client.GopestatWeb;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.FormErrorOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;

public class NewInstanceForm extends DynamicForm {

    private RequiredTextItem identifier;
    private RequiredTextItem title;
    private ButtonItem       saveButton;

    public NewInstanceForm() {
        super();

        identifier = new RequiredTextItem("in-id", GopestatWeb.getConstants().instanceIdentifier());
        identifier.setWidth(200);

        title = new RequiredTextItem("in-title", GopestatWeb.getConstants().instanceTitle());
        title.setWidth(200);

        saveButton = new ButtonItem("op-save", GopestatWeb.getConstants().actionCreateInstance());
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
