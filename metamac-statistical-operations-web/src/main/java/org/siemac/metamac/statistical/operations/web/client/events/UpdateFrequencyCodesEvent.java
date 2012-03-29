package org.siemac.metamac.statistical.operations.web.client.events;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateFrequencyCodesEvent extends GwtEvent<UpdateFrequencyCodesEvent.UpdateFrequencyCodesHandler> {

    public interface UpdateFrequencyCodesHandler extends EventHandler {

        void onUpdateFrequencyCodes(UpdateFrequencyCodesEvent event);
    }

    private static Type<UpdateFrequencyCodesHandler> TYPE = new Type<UpdateFrequencyCodesHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateFrequencyCodesHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<ExternalItemBtDto> updateFrequencyCodes, List<ExternalItemBtDto> temporalGranularityCodes, List<ExternalItemBtDto> freqCollCodes) {
        if (TYPE != null) {
            source.fireEvent(new UpdateFrequencyCodesEvent(updateFrequencyCodes, temporalGranularityCodes, freqCollCodes));
        }
    }

    @Override
    protected void dispatch(UpdateFrequencyCodesHandler handler) {
        handler.onUpdateFrequencyCodes(this);
    }

    private final List<ExternalItemBtDto> updateFrequencyCodes;
    private final List<ExternalItemBtDto> temporalGranularityCodes;
    private final List<ExternalItemBtDto> freqCollCodes;

    public UpdateFrequencyCodesEvent(List<ExternalItemBtDto> updateFrequencyCodes, List<ExternalItemBtDto> temporalGranularityCodes, List<ExternalItemBtDto> freqCollCodes) {
        this.updateFrequencyCodes = updateFrequencyCodes;
        this.temporalGranularityCodes = temporalGranularityCodes;
        this.freqCollCodes = freqCollCodes;
    }

    public List<ExternalItemBtDto> getUpdateFrequencyCodes() {
        return updateFrequencyCodes;
    }

    public List<ExternalItemBtDto> getTemporalGranularityCodes() {
        return temporalGranularityCodes;
    }

    public List<ExternalItemBtDto> getFreqCollCodes() {
        return freqCollCodes;
    }

    public static Type<UpdateFrequencyCodesHandler> getType() {
        return TYPE;
    }

}
