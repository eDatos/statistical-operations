package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.conf.ConfigurationServiceImpl;
import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.GetFrequencyCodesAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFrequencyCodesResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetFrequencyCodesActionHandler extends AbstractActionHandler<GetFrequencyCodesAction, GetFrequencyCodesResult> {

    private static final String      OPERATION_UPDATE_FREQUENCY    = "metamac.gopestast.codelist.operation.updatefrequency";
    private static final String      INSTANCE_TEMPORAL_GRANULARITY = "metamac.gopestast.codelist.instance.temporalgranularity";
    private static final String      INSTANCE_FREQ_COLL            = "metamac.gopestast.codelist.instance.freqcoll";

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public GetFrequencyCodesActionHandler() {
        super(GetFrequencyCodesAction.class);
    }

    @Override
    public GetFrequencyCodesResult execute(GetFrequencyCodesAction action, ExecutionContext context) throws ActionException {
        // CodeLists URIs
        String updateFrequency = ((ConfigurationServiceImpl) ApplicationContextProvider.getApplicationContext().getBean("configurationService")).getProperties()
                .getProperty(OPERATION_UPDATE_FREQUENCY);
        String temporalGranularity = ((ConfigurationServiceImpl) ApplicationContextProvider.getApplicationContext().getBean("configurationService")).getProperties().getProperty(
                INSTANCE_TEMPORAL_GRANULARITY);
        String freqColl = ((ConfigurationServiceImpl) ApplicationContextProvider.getApplicationContext().getBean("configurationService")).getProperties().getProperty(INSTANCE_FREQ_COLL);
        // Codes
        List<ExternalItemBtDto> updateFrequencyCodes = metamacCoreCommonService.retrieveCodelist(ServiceContextHolder.getCurrentServiceContext(), updateFrequency);
        List<ExternalItemBtDto> temporalGranularityCodes = metamacCoreCommonService.retrieveCodelist(ServiceContextHolder.getCurrentServiceContext(), temporalGranularity);
        List<ExternalItemBtDto> freqCollCodes = metamacCoreCommonService.retrieveCodelist(ServiceContextHolder.getCurrentServiceContext(), freqColl);
        return new GetFrequencyCodesResult(updateFrequencyCodes, temporalGranularityCodes, freqCollCodes);
    }

    @Override
    public void undo(GetFrequencyCodesAction action, GetFrequencyCodesResult result, ExecutionContext context) throws ActionException {

    }

}
