package org.siemac.metamac.statistical.operations.web.server.handlers.external;

import java.util.ArrayList;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.ResourceInternal;
import org.siemac.metamac.statistical.operations.web.server.rest.CommonMetadataRestExternalFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.utils.ExternalItemUtils;
import org.siemac.metamac.statistical.operations.web.shared.external.GetCommonMetadataConfigurationsAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetCommonMetadataConfigurationsResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetCommonMetadataConfigurationsActionHandler extends SecurityActionHandler<GetCommonMetadataConfigurationsAction, GetCommonMetadataConfigurationsResult> {

    @Autowired
    private CommonMetadataRestExternalFacade commonMetadataRestExternalFacade;

    public GetCommonMetadataConfigurationsActionHandler() {
        super(GetCommonMetadataConfigurationsAction.class);
    }

    @Override
    public GetCommonMetadataConfigurationsResult executeSecurityAction(GetCommonMetadataConfigurationsAction action) throws ActionException {

        ServiceContext serviceContext = ServiceContextHolder.getCurrentServiceContext();

        Configurations result = commonMetadataRestExternalFacade.findConfigurations(serviceContext, action.getCriteria());
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>();
        if (result != null && result.getConfigurations() != null) {
            for (ResourceInternal resource : result.getConfigurations()) {
                externalItemDtos.add(ExternalItemUtils.getConfigurationAsExternalItemDto(resource));
            }
        }
        return new GetCommonMetadataConfigurationsResult(externalItemDtos);
    }
}
