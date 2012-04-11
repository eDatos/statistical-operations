package org.siemac.metamac.statistical.operations.web.client.events;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateOrganisationSchemesEvent extends GwtEvent<UpdateOrganisationSchemesEvent.UpdateOrganisationSchemesHandler> {

    public interface UpdateOrganisationSchemesHandler extends EventHandler {

        void onUpdateOrganisationSchemes(UpdateOrganisationSchemesEvent event);
    }

    private static Type<UpdateOrganisationSchemesHandler> TYPE = new Type<UpdateOrganisationSchemesHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateOrganisationSchemesHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<ExternalItemBtDto> categorySchemes) {
        if (TYPE != null) {
            source.fireEvent(new UpdateOrganisationSchemesEvent(categorySchemes));
        }
    }

    @Override
    protected void dispatch(UpdateOrganisationSchemesHandler handler) {
        handler.onUpdateOrganisationSchemes(this);
    }

    private final List<ExternalItemBtDto> organisationSchemes;

    public UpdateOrganisationSchemesEvent(List<ExternalItemBtDto> categorySchemes) {
        this.organisationSchemes = categorySchemes;
    }

    public List<ExternalItemBtDto> getOrganisationSchemes() {
        return organisationSchemes;
    }

    public static Type<UpdateOrganisationSchemesHandler> getType() {
        return TYPE;
    }

}
