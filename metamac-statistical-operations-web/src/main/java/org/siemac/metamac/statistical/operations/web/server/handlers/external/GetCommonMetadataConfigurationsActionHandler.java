package org.siemac.metamac.statistical.operations.web.server.handlers.external;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.constants.shared.ConfigurationConstants;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.statistical.operations.web.server.rest.CommonMetadataRestExternalFacade;
import org.siemac.metamac.statistical.operations.web.shared.external.GetCommonMetadataConfigurationsAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetCommonMetadataConfigurationsResult;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.DtoUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetCommonMetadataConfigurationsActionHandler extends SecurityActionHandler<GetCommonMetadataConfigurationsAction, GetCommonMetadataConfigurationsResult> {

    @Autowired
    private CommonMetadataRestExternalFacade commonMetadataRestExternalFacade;

    @Autowired
    private ConfigurationService             configurationService;

    public GetCommonMetadataConfigurationsActionHandler() {
        super(GetCommonMetadataConfigurationsAction.class);
    }

    @Override
    public GetCommonMetadataConfigurationsResult executeSecurityAction(GetCommonMetadataConfigurationsAction action) throws ActionException {

        String commonMetadataRestApiEndpoint = configurationService.getProperty(ConfigurationConstants.ENDPOINT_COMMON_METADATA_EXTERNAL_API);

        Configurations result = commonMetadataRestExternalFacade.findConfigurations(action.getQuery());
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>();
        if (result != null && result.getConfigurations() != null) {
            for (Resource resource : result.getConfigurations()) {
                // Do not store rest api endpoint
                String uri = StringUtils.removeStart(resource.getSelfLink().getHref(), commonMetadataRestApiEndpoint);
                ExternalItemDto externalItemDto = new ExternalItemDto(resource.getId(), uri, resource.getUrn(), TypeExternalArtefactsEnum.CONFIGURATION,
                        DtoUtils.getInternationalStringDtoFromInternationalString(resource.getTitle()));
                externalItemDtos.add(externalItemDto);
            }
        }
        return new GetCommonMetadataConfigurationsResult(externalItemDtos);
    }
}
