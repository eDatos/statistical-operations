package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.statistical.operations.web.server.rest.CommonMetadataRestExternalFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetCommonMetadataConfigurationsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCommonMetadataConfigurationsResult;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.DtoUtils;
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
        Configurations result = commonMetadataRestExternalFacade.findConfigurations(action.getQuery());
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>();
        if (result != null && result.getConfigurations() != null) {
            for (Resource resource : result.getConfigurations()) {
                // TODO NO GUARDAR EL ENDPOINT DE LA API
                ExternalItemDto externalItemDto = new ExternalItemDto(resource.getId(), resource.getSelfLink().getHref(), resource.getUrn(), TypeExternalArtefactsEnum.STATISTICAL_OPERATION,
                        DtoUtils.getInternationalStringDtoFromInternationalString(resource.getTitle()));
                externalItemDtos.add(externalItemDto);
            }
        }
        return new GetCommonMetadataConfigurationsResult(externalItemDtos);
    }

}
